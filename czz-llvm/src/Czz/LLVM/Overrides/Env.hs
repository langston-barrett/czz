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

module Czz.LLVM.Overrides.Env
  ( Effect(..)
  , GetEnvEffect(..)
  , overrides
  )
where

import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (liftIO)
import qualified Data.BitVector.Sized as BV
import           Data.ByteString as BS
import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import           Data.Text (Text)
import qualified Data.Text as Text

import qualified What4.Interface as What4

-- crucible
import           Lang.Crucible.Backend (IsSymBackend)
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.Simulator.RegMap as C
import           Lang.Crucible.Simulator.OverrideSim (OverrideSim)
import qualified Lang.Crucible.Simulator as C
import           Lang.Crucible.Simulator.RegMap (RegEntry, RegValue)

-- crucible-llvm
import qualified Lang.Crucible.LLVM.DataLayout as CLLVM
import           Lang.Crucible.LLVM.Intrinsics (OverrideTemplate)
import qualified Lang.Crucible.LLVM.Intrinsics as CLLVM
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import           Lang.Crucible.LLVM.MemModel.Pointer (LLVMPointerType)

import qualified Czz.Log as Log
import           Czz.Overrides (EffectTrace)
import qualified Czz.Overrides as COv

import           Czz.LLVM.Overrides.State.Env as State.Env
import           Czz.LLVM.Overrides.Util (OverrideConstraints)
import           Czz.LLVM.QQ (llvmOvr, llvmOvrType)

data Effect
  = GetEnv !GetEnvEffect
  deriving (Eq, Ord, Show)

_GetEnv :: Lens.Prism' Effect GetEnvEffect
_GetEnv =
  Lens.prism'
    GetEnv
    (\case
      GetEnv eff -> Just eff)
      -- _ -> Nothing)

overrides ::
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff Effect ->
  IORef [ByteString] ->
  C.GlobalVar State.Env.EnvState ->
  [OverrideTemplate p sym arch rtp l a]
overrides proxy effects inj envVarRef envStateVar =
  [ ov (getEnvDecl proxy effects (inj . _GetEnv) envVarRef envStateVar)
  ]
  where ov = CLLVM.basic_llvm_override

------------------------------------------------------------------------
-- ** utilities

------------------------------------------------------------------------
-- ** getenv

data GetEnvEffect
  = GetEnvEffect
  deriving (Eq, Ord, Show)

-- | NB: Allocates fresh results every time.
getEnvDecl ::
  forall proxy p sym arch wptr eff.
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff GetEnvEffect ->
  IORef [ByteString] ->
  C.GlobalVar State.Env.EnvState ->
  [llvmOvrType| i8* @( i8* ) |]
getEnvDecl proxy effects inj envVarRef envStateVar =
  [llvmOvr| i8* @getenv( i8* ) |]
  (\memVar bak args ->
    COv.toOverride
      @(LLVMPointerType wptr)
      effects
      inj
      args
      (COv.Override
       { COv.genEffect = \_proxy _oldEff _name ->
           COv.AnyOverrideSim (return GetEnvEffect)
       , COv.doEffect = \_proxy e name ->
           COv.AnyOverrideSim (getEnvImpl proxy bak e envVarRef memVar envStateVar name)
       }))

getEnvImpl ::
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  GetEnvEffect ->
  IORef [ByteString] ->
  C.GlobalVar CLLVM.Mem ->
  C.GlobalVar State.Env.EnvState ->
  RegEntry sym (LLVMPointerType wptr) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (LLVMPointerType wptr))
getEnvImpl _proxy bak e envVarRef memVar envStateVar varNamePtr = do
  GetEnvEffect <- return e  -- check for missing pattern matches
  let sym = C.backendGetSym bak
  C.modifyGlobal memVar $ \mem -> do
    let ptr = C.regValue varNamePtr
    varNameStr <- liftIO (BS.pack <$> CLLVM.loadString bak mem ptr Nothing)
    liftIO (Log.debug ("Program called `getenv`: " <> Text.pack (show varNameStr)))
    liftIO (IORef.modifyIORef envVarRef (varNameStr:))
    State.Env.getEnv bak envStateVar varNameStr >>=
      \case
        Nothing -> do
          nullPtr <- liftIO (CLLVM.mkNullPointer sym ?ptrWidth)
          return (nullPtr, mem)
        Just str -> do
          -- TODO(lb): lift allocate-then-write-bytestring to a helper
          -- NB: This is already null-terminated, since `setenv` requires that,
          -- and the initial values are CStrings.
          let val = CLLVM.LLVMValString str
          let ty = CLLVM.llvmValStorableType val
          -- TODO(lb): check for truncation
          let len = BS.length str
          let lenBv = BV.mkBV ?ptrWidth (toEnum len)
          strLen <- liftIO (What4.bvLit sym ?ptrWidth lenBv)
          -- Allocate space for the argument string
          let nm = "czz:getenv-str"
          (strPtr, mem') <- liftIO (CLLVM.doAlloca bak mem strLen CLLVM.noAlignment nm)
          mem'' <- liftIO (CLLVM.storeRaw bak mem' strPtr ty CLLVM.noAlignment val)
          return (strPtr, mem'')
