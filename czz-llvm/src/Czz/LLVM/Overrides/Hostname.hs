{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Czz.LLVM.Overrides.Hostname
  ( Effect(..)
  , GetHostNameEffect(..)
  , overrides
  )
where

import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (liftIO)
import qualified Data.BitVector.Sized as BV
import qualified Data.ByteString as BS
import           Data.IORef (IORef)
import           Data.Text (Text)

import           Data.Parameterized.NatRepr (knownNat)

import qualified What4.Interface as What4

-- crucible
import           Lang.Crucible.Backend (IsSymBackend)
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.Simulator.RegMap as C
import           Lang.Crucible.Simulator.OverrideSim (OverrideSim)
import qualified Lang.Crucible.Simulator as C
import           Lang.Crucible.Simulator.RegMap (RegEntry, RegValue)
import           Lang.Crucible.Types (BVType)

-- crucible-llvm
import qualified Lang.Crucible.LLVM.DataLayout as CLLVM
import           Lang.Crucible.LLVM.Intrinsics (OverrideTemplate)
import qualified Lang.Crucible.LLVM.Intrinsics as CLLVM
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import           Lang.Crucible.LLVM.MemModel.Pointer (LLVMPointerType)

import qualified Czz.Log as Log
import           Czz.Overrides (EffectTrace)
import qualified Czz.Overrides as COv

import           Czz.LLVM.Overrides.Util (OverrideConstraints)
import           Czz.LLVM.QQ (llvmOvr, llvmOvrType)
import qualified Czz.LLVM.Unimplemented as Unimpl

data Effect
  = GetHostName !GetHostNameEffect
  deriving (Eq, Ord, Show)

_GetHostName :: Lens.Prism' Effect GetHostNameEffect
_GetHostName =
  Lens.prism'
    GetHostName
    (\case
      GetHostName eff -> Just eff)
      -- _ -> Nothing)

overrides ::
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff Effect ->
  [OverrideTemplate p sym arch rtp l a]
overrides proxy effects inj =
  [ ov (getHostNameDecl proxy effects (inj . _GetHostName))
  ]
  where ov = CLLVM.basic_llvm_override

------------------------------------------------------------------------
-- ** gethostname

data GetHostNameEffect
  = GetHostNameSuccess
  deriving (Eq, Ord, Show)

getHostNameDecl ::
  forall proxy p sym arch wptr eff.
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff GetHostNameEffect ->
  [llvmOvrType| i32 @(i8*, size_t) |]
getHostNameDecl proxy effects inj =
  [llvmOvr| i32 @gethostname( i8*, size_t ) |]
  (\memVar bak args ->
    COv.toOverride
      @(BVType 32)
      effects
      inj
      args
      (COv.Override
       { COv.genEffect = \_proxy _oldEff _ptr _len ->
           COv.AnyOverrideSim (return GetHostNameSuccess)
       , COv.doEffect = \_proxy e ptr len ->
           COv.AnyOverrideSim (getHostNameImpl proxy bak e memVar ptr len)
       }))

-- | Unsound!
--
-- TODO(lb): also generate error conditions
getHostNameImpl ::
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  GetHostNameEffect ->
  C.GlobalVar CLLVM.Mem ->
  RegEntry sym (LLVMPointerType wptr) ->
  RegEntry sym (BVType wptr) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (BVType 32))
getHostNameImpl _proxy bak e memVar (C.regValue -> ptr) len = do
  GetHostNameSuccess <- return e  -- check for missing pattern matches
  let sym = C.backendGetSym bak
  let hostname = "hostname"
  let lenLt bv =
        liftIO (What4.bvSlt sym (C.regValue len) =<< What4.bvLit sym ?ptrWidth bv)
  lenNeg <- lenLt (BV.mkBV ?ptrWidth 0)
  -- NOTE(lb): It isn't currently necessary to check if ?ptrWidth is wide
  -- enough to hold the hostname because the hostname is small and fixed, and
  -- the ArchOk constraint guarantees that the pointer width is at least 16.
  -- if this override is changed to e.g. use really long hostnames it might
  -- be necessary to check that mkBV doesn't truncate the length here.
  lenSmall <- lenLt (BV.mkBV ?ptrWidth (toEnum (BS.length hostname)))
  C.symbolicBranches
    C.emptyRegMap
    [ ( lenNeg
      , Unimpl.throw Unimpl.GetHostNameNegativeSize
      , Nothing
      )
    , ( lenSmall
      , Unimpl.throw Unimpl.GetHostNameSmallSize
      , Nothing
      )
    , -- TODO Check for name size
      ( What4.truePred sym,
        C.modifyGlobal memVar $ \mem -> liftIO $ do
          let val = CLLVM.LLVMValString hostname
          let ty = CLLVM.llvmValStorableType val
          mem1 <- CLLVM.storeRaw bak mem ptr ty CLLVM.noAlignment val
          bv0' <- What4.bvLit sym (knownNat @32) (BV.mkBV (knownNat @32) 0)
          return (bv0', mem1)
      , Nothing
      )
    ]
