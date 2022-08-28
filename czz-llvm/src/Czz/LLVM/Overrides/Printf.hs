{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Czz.LLVM.Overrides.Printf
  ( Effect(..)
  , SprintfEffect(..)
  , overrides
  )
where

import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State as State
import qualified Data.BitVector.Sized as BV
import qualified Data.ByteString.Char8 as BS
import           Data.IORef (IORef)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word8)

import           Data.Parameterized.NatRepr (knownNat)

import qualified What4.Interface as What4

-- crucible
import           Lang.Crucible.Backend (IsSymBackend)
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.Simulator.RegMap as C
import           Lang.Crucible.Simulator.OverrideSim (OverrideSim)
import qualified Lang.Crucible.Simulator as C
import           Lang.Crucible.Simulator.RegMap (RegEntry, RegValue)
import           Lang.Crucible.Types (AnyType, BVType, VectorType)

-- crucible-llvm
import qualified Lang.Crucible.LLVM.DataLayout as CLLVM
import           Lang.Crucible.LLVM.Intrinsics (OverrideTemplate)
import qualified Lang.Crucible.LLVM.Intrinsics as CLLVM
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import           Lang.Crucible.LLVM.MemModel.Pointer (LLVMPointerType)
import qualified Lang.Crucible.LLVM.Printf as CLLVM

import qualified Czz.Log as Log
import           Czz.Overrides (EffectTrace)
import qualified Czz.Overrides as COv

import           Czz.LLVM.Overrides.Util (OverrideConstraints)
import           Czz.LLVM.QQ (llvmOvr, llvmOvrType)

data Effect
  = Sprintf !SprintfEffect
  deriving (Eq, Ord, Show)

_Sprintf :: Lens.Prism' Effect SprintfEffect
_Sprintf =
  Lens.prism'
    Sprintf
    (\case
      Sprintf eff -> Just eff)
      -- _ -> Nothing)

overrides ::
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff Effect ->
  [OverrideTemplate p sym arch rtp l a]
overrides proxy effects inj =
  [ ov (sprintfDecl proxy effects (inj . _Sprintf))
  ]
  where ov = CLLVM.basic_llvm_override

------------------------------------------------------------------------
-- ** sprintf

data SprintfEffect
  = SprintfSuccess
  deriving (Eq, Ord, Show)

sprintfDecl ::
  forall proxy p sym arch wptr eff.
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff SprintfEffect ->
  [llvmOvrType| i32 @( i8*, i8*, ... ) |]
sprintfDecl proxy effects inj =
  [llvmOvr| i32 @sprintf( i8*, i8*, ... ) |]
  (\memVar bak args ->
    COv.toOverride
      @(BVType 32)
      effects
      inj
      args
      (COv.Override
       { COv.genEffect = \_proxy _oldEff _str _fmt _vaList ->
           COv.AnyOverrideSim (return SprintfSuccess)
       , COv.doEffect = \_proxy e  str fmt vaList ->
           COv.AnyOverrideSim (sprintfImpl proxy bak e memVar str fmt vaList)
       }))

runFmtString ::
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  CLLVM.MemImpl sym ->
  [Word8] ->
  RegValue sym (VectorType AnyType) ->
  IO (Either String ((String, Int), CLLVM.MemImpl sym))
runFmtString _proxy bak mem fmt vaList =
  case CLLVM.parseDirectives fmt of
    Left err -> return (Left err)
    Right directives ->
      Right <$>
        State.runStateT
          (CLLVM.executeDirectives (CLLVM.printfOps bak vaList) directives)
          mem

overrideRunFmtString ::
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  C.GlobalVar CLLVM.Mem ->
  RegEntry sym (LLVMPointerType wptr) ->
  RegEntry sym (VectorType AnyType) ->
  OverrideSim p sym ext rtp args ret (String, Int)
overrideRunFmtString proxy bak memVar fmtPtr vaList = do
  mem0 <- C.readGlobal memVar
  fmt <- liftIO $ CLLVM.loadString bak mem0 (C.regValue fmtPtr) Nothing
  C.modifyGlobal memVar $ \mem ->
    liftIO (runFmtString proxy bak mem fmt (C.regValue vaList)) >>=
      \case
        Left err ->
          C.overrideError $
            C.AssertFailureSimError "Format string parsing failed" err
        Right ret -> return ret

-- | Unsound!
--
-- TODO(lb): also generate error conditions
sprintfImpl ::
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  SprintfEffect ->
  C.GlobalVar CLLVM.Mem ->
  RegEntry sym (LLVMPointerType wptr) ->
  RegEntry sym (LLVMPointerType wptr) ->
  RegEntry sym (VectorType AnyType) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (BVType 32))
sprintfImpl proxy bak e memVar (C.regValue -> str) fmt vaList = do
  SprintfSuccess <- return e  -- check for missing pattern matches
  let sym = C.backendGetSym bak
  (formatted, n) <- overrideRunFmtString proxy bak memVar fmt vaList
  liftIO (Log.debug ("Program called `sprintf`: " <> Text.pack formatted))
  -- TODO(lb): null-terminated?
  let val = CLLVM.LLVMValString (BS.pack formatted)
  let ty = CLLVM.llvmValStorableType val
  -- TODO(lb): check for truncation
  -- TODO(lb): is storeRaw correct?
  () <-
    C.modifyGlobal memVar $ \mem -> liftIO $ do
      ((),) <$> CLLVM.storeRaw bak mem str ty CLLVM.noAlignment val
  liftIO $ What4.bvLit sym knownNat (BV.mkBV knownNat (toInteger n))

-- TODO(lb): snprintf
