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

module Czz.LLVM.Overrides.Time
  ( Effect(..)
  , GetTimeOfDayEffect(..)
  , overrides
  )
where

import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as AesonTH
import qualified Data.BitVector.Sized as BV
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

data GetTimeOfDayEffect
  = GetTimeOfDayEffect
  deriving (Eq, Ord, Show)

data TimeEffect
  = TimeEffect
  deriving (Eq, Ord, Show)

data Effect
  = GetTimeOfDay !GetTimeOfDayEffect
  | Time !TimeEffect
  deriving (Eq, Ord, Show)

$(Lens.makePrisms ''Effect)
$(AesonTH.deriveJSON Aeson.defaultOptions ''GetTimeOfDayEffect)
$(AesonTH.deriveJSON Aeson.defaultOptions ''TimeEffect)
$(AesonTH.deriveJSON Aeson.defaultOptions ''Effect)

overrides ::
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff Effect ->
  [OverrideTemplate p sym arch rtp l a]
overrides proxy effects inj =
  [ ov (getTimeOfDayDecl proxy effects (inj . _GetTimeOfDay))
  , ov (timeDecl proxy effects (inj . _Time))
  ]
  where ov = CLLVM.basic_llvm_override

------------------------------------------------------------------------
-- ** gettimeofday

getTimeOfDayDecl ::
  forall proxy p sym arch wptr eff.
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff GetTimeOfDayEffect ->
  [llvmOvrType| i32 @( %struct.timeval*, i8* ) |]
getTimeOfDayDecl proxy effects inj =
  [llvmOvr| i32 @gettimeofday( %struct.timeval*, i8* ) |]
  (\memVar bak args ->
    COv.toOverride
      @(BVType 32)
      effects
      inj
      args
      (COv.Override
       { COv.genEffect = \_proxy _oldEff _tv _tz ->
           COv.AnyOverrideSim (return GetTimeOfDayEffect)
       , COv.doEffect = \_proxy e tv tz ->
           COv.AnyOverrideSim (getTimeOfDayImpl proxy bak e memVar tv tz)
       }))

-- | Unsound!
--
-- TODO(lb): also generate error conditions
getTimeOfDayImpl ::
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  GetTimeOfDayEffect ->
  C.GlobalVar CLLVM.Mem ->
  RegEntry sym (LLVMPointerType wptr) ->
  RegEntry sym (LLVMPointerType wptr) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (BVType 32))
getTimeOfDayImpl _proxy bak e memVar tv tz = do
  GetTimeOfDayEffect <- return e  -- check for missing pattern matches
  let sym = C.backendGetSym bak
  tzNonNull <-
    liftIO (What4.notPred sym =<< CLLVM.ptrIsNull sym ?ptrWidth (C.regValue tz))
  liftIO (Log.debug "In `gettimeofday`")
  C.symbolicBranches
    C.emptyRegMap
    [ ( tzNonNull
      , Unimpl.throw Unimpl.GetTimeOfDayNonNullTz
      , Nothing
      )
    , ( What4.truePred sym,
        -- TODO(lb): Randomize time?
        C.modifyGlobal memVar $ \mem -> liftIO $ do
          -- %struct.timeval = type { i64, i64 }
          zero8 <- What4.bvLit sym (BV.knownNat @8) (BV.mkBV (BV.knownNat @8) 0)
          let ptr = C.regValue tv
          -- 16 = 8 + 8 = sizeof(i64) + sizeof(i64)
          sz <- What4.bvLit sym (BV.knownNat @64) (BV.mkBV (BV.knownNat @64) 16)
          mem' <- CLLVM.doMemset bak (knownNat @64) mem ptr zero8 sz
          zero32 <- What4.bvLit sym (BV.knownNat @32) (BV.mkBV (BV.knownNat @32) 0)
          return (zero32, mem')
      , Nothing
      )
    ]

------------------------------------------------------------------------
-- ** time

timeDecl ::
  forall proxy p sym arch wptr eff.
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff TimeEffect ->
  [llvmOvrType| size_t @( size_t* ) |]
timeDecl proxy effects inj =
  [llvmOvr| size_t @time( size_t* ) |]
  (\memVar bak args ->
    COv.toOverride
      @(BVType wptr)
      effects
      inj
      args
      (COv.Override
       { COv.genEffect = \_proxy _oldEff _tloc ->
           COv.AnyOverrideSim (return TimeEffect)
       , COv.doEffect = \_proxy e tloc ->
           COv.AnyOverrideSim (timeImpl proxy bak e memVar tloc)
       }))

timeImpl ::
  forall proxy arch p sym bak ext rtp args ret wptr.
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  TimeEffect ->
  C.GlobalVar CLLVM.Mem ->
  RegEntry sym (LLVMPointerType wptr) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (BVType wptr))
timeImpl _proxy bak e _memVar tloc = do
  TimeEffect <- return e  -- check for missing pattern matches
  let sym = C.backendGetSym bak
  tlocNonNull <-
    liftIO (What4.notPred sym =<< CLLVM.ptrIsNull sym ?ptrWidth (C.regValue tloc))
  liftIO (Log.debug "In `time`")
  C.symbolicBranches
    C.emptyRegMap
    [ ( tlocNonNull
      , Unimpl.throw Unimpl.TimeLocNonNull
      , Nothing
      )
    , ( What4.truePred sym,
        -- TODO(lb): Randomize time?
        liftIO (What4.bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth 0))
      , Nothing
      )
    ]
