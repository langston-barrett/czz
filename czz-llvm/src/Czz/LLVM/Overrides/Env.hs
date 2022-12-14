{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as AesonTH
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

import qualified Czz.LLVM.Overrides.Errno as Errno
import           Czz.LLVM.Overrides.State.Env as EnvState
import           Czz.LLVM.Overrides.Util (OverrideConstraints)
import           Czz.LLVM.QQ (llvmOvr, llvmOvrType)

data GetEnvEffect
  = GetEnvEffect
  deriving (Eq, Ord, Show)

data UnsetEnvEffect
  = UnsetEnvEffect
  deriving (Eq, Ord, Show)

data Effect
  = GetEnv !GetEnvEffect
  | UnsetEnv !UnsetEnvEffect
  deriving (Eq, Ord, Show)

$(Lens.makePrisms ''Effect)
$(AesonTH.deriveJSON Aeson.defaultOptions ''GetEnvEffect)
$(AesonTH.deriveJSON Aeson.defaultOptions ''UnsetEnvEffect)
$(AesonTH.deriveJSON Aeson.defaultOptions ''Effect)

overrides ::
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff Effect ->
  IORef [ByteString] ->
  C.GlobalVar EnvState ->
  [OverrideTemplate p sym arch rtp l a]
overrides proxy effects inj envVarRef envStateVar =
  [ ov (getEnvDecl proxy effects (inj . _GetEnv) envVarRef envStateVar)
  , ov (unsetEnvDecl proxy effects (inj . _UnsetEnv) envVarRef envStateVar)
  ]
  where ov = CLLVM.basic_llvm_override

------------------------------------------------------------------------
-- ** getenv

-- | NB: Allocates fresh results every time.
getEnvDecl ::
  forall proxy p sym arch wptr eff.
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff GetEnvEffect ->
  IORef [ByteString] ->
  C.GlobalVar EnvState ->
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
  C.GlobalVar EnvState ->
  RegEntry sym (LLVMPointerType wptr) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (LLVMPointerType wptr))
getEnvImpl _proxy bak e envVarRef memVar envStateVar varNamePtr = do
  GetEnvEffect <- return e  -- check for missing pattern matches
  let sym = C.backendGetSym bak
  C.modifyGlobal memVar $ \mem -> do
    let ptr = C.regValue varNamePtr
    varNameStr <- liftIO (BS.pack <$> CLLVM.loadString bak mem ptr Nothing)
    let showVarName = Text.pack (show varNameStr)
    liftIO (Log.debug ("Program called `getenv`: " <> showVarName))
    liftIO (IORef.modifyIORef envVarRef (varNameStr:))
    EnvState.getEnv bak envStateVar varNameStr >>=
      \case
        Nothing -> do
          liftIO (Log.debug ("getenv(" <> showVarName <> ") = null"))
          nullPtr <- liftIO (CLLVM.mkNullPointer sym ?ptrWidth)
          return (nullPtr, mem)
        Just str -> do
          liftIO (Log.debug ("getenv(" <> showVarName <> ") = " <> Text.pack (show str)))
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

------------------------------------------------------------------------
-- ** unsetEnv

-- | NB: Allocates fresh results every time.
unsetEnvDecl ::
  forall proxy p sym arch wptr eff.
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff UnsetEnvEffect ->
  IORef [ByteString] ->
  C.GlobalVar EnvState ->
  [llvmOvrType| i32 @( i8* ) |]
unsetEnvDecl proxy effects inj envVarRef envStateVar =
  [llvmOvr| i32 @unsetenv( i8* ) |]
  (\memVar bak args ->
    COv.toOverride
      @(BVType 32)
      effects
      inj
      args
      (COv.Override
       { COv.genEffect = \_proxy _oldEff _name ->
           COv.AnyOverrideSim (return UnsetEnvEffect)
       , COv.doEffect = \_proxy e name ->
           COv.AnyOverrideSim (unsetEnvImpl proxy bak e envVarRef memVar envStateVar name)
       }))

-- TODO(lb): EINVAL if name is NULL
-- TODO(lb): model failure
unsetEnvImpl ::
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  UnsetEnvEffect ->
  IORef [ByteString] ->
  C.GlobalVar CLLVM.Mem ->
  C.GlobalVar EnvState ->
  RegEntry sym (LLVMPointerType wptr) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (BVType 32))
unsetEnvImpl _proxy bak e _envVarRef memVar envStateVar varNamePtr = do
  UnsetEnvEffect <- return e  -- check for missing pattern matches
  let sym = C.backendGetSym bak
  mem <- C.readGlobal memVar
  let ptr = C.regValue varNamePtr
  varNamePtrIsNull <-
    liftIO (CLLVM.ptrIsNull sym ?ptrWidth (C.regValue varNamePtr))
  let n32 = What4.knownNat @32
  C.symbolicBranches
    C.emptyRegMap
    [ ( varNamePtrIsNull
      , do Errno.setErrno (Just sym) bak memVar Errno.einval
           liftIO (What4.bvLit sym n32 (BV.mkBV n32 (-1)))
      , Nothing
      )
    , ( What4.truePred sym
      , do varNameStr <- liftIO (BS.pack <$> CLLVM.loadString bak mem ptr Nothing)
           let showVarName = Text.pack (show varNameStr)
           liftIO (Log.debug ("unsetenv(" <> showVarName <> ") = 0"))
           ret <- EnvState.unsetEnv bak envStateVar varNameStr
           liftIO (What4.bvLit sym n32 (BV.mkBV n32 (toInteger ret)))
      , Nothing
      )
    ]
