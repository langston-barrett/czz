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

module Czz.LLVM.Overrides.Signal
  ( Effect(..)
  , SignalEffect(..)
  , overrides
  )
where

import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (IORef)
import           Data.Text (Text)

-- crucible
import           Lang.Crucible.Backend (IsSymBackend)
import qualified Lang.Crucible.Backend as C
import           Lang.Crucible.Simulator.OverrideSim (OverrideSim)
import qualified Lang.Crucible.Simulator as C
import           Lang.Crucible.Simulator.RegMap (RegEntry, RegValue)
import           Lang.Crucible.Types (BVType)

-- crucible-llvm
import           Lang.Crucible.LLVM.Intrinsics (OverrideTemplate)
import qualified Lang.Crucible.LLVM.Intrinsics as CLLVM
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import           Lang.Crucible.LLVM.MemModel.Pointer (LLVMPointerType)

import qualified Czz.Log as Log
import           Czz.Overrides (EffectTrace)
import qualified Czz.Overrides as COv

import           Czz.LLVM.Overrides.Util (OverrideConstraints)
import           Czz.LLVM.QQ (llvmOvr, llvmOvrType)

data Effect
  = Signal !SignalEffect
  deriving (Eq, Ord, Show)

_Signal :: Lens.Prism' Effect SignalEffect
_Signal =
  Lens.prism'
    Signal
    (\case
      Signal eff -> Just eff)
      -- _ -> Nothing)

overrides ::
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff Effect ->
  [OverrideTemplate p sym arch rtp l a]
overrides proxy effects inj =
  [ ov (signalDecl proxy effects (inj . _Signal))
  ]
  where ov = CLLVM.basic_llvm_override

------------------------------------------------------------------------
-- ** signal

data SignalEffect
  = SignalEffect
  deriving (Eq, Ord, Show)

signalDecl ::
  forall proxy p sym arch wptr eff.
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff SignalEffect ->
  [llvmOvrType| void (i32)* @( i32, void (i32)* ) |]
signalDecl proxy effects inj =
  [llvmOvr| void (i32)* @signal( i32, void (i32)* ) |]
  (\memVar bak args ->
    COv.toOverride
      @(LLVMPointerType wptr)
      effects
      inj
      args
      (COv.Override
       { COv.genEffect = \_proxy _oldEff _sig _handler ->
           COv.AnyOverrideSim (return SignalEffect)
       , COv.doEffect = \_proxy e sig handler ->
           COv.AnyOverrideSim (signalImpl proxy bak e memVar sig handler)
       }))

-- | TODO(lb): super unsound!
signalImpl ::
  forall proxy arch p sym bak ext rtp args ret wptr.
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  SignalEffect ->
  C.GlobalVar CLLVM.Mem ->
  RegEntry sym (BVType 32) ->
  RegEntry sym (LLVMPointerType wptr) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (LLVMPointerType wptr))
signalImpl _proxy bak e _memVar _sig _handler = do
  SignalEffect <- return e  -- check for missing pattern matches
  let sym = C.backendGetSym bak
  liftIO (CLLVM.mkNullPointer sym ?ptrWidth)
