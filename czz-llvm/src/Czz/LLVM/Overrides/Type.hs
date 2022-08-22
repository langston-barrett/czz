{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

module Czz.LLVM.Overrides.Type
  ( Override(..)
  , toLLVMOverride
  )
where

import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (IORef)
import qualified Data.IORef as IORef

-- p-u
import qualified Data.Parameterized.Context as Ctx

-- crucible
import           Lang.Crucible.Backend (IsSymBackend)
import           Lang.Crucible.Simulator.OverrideSim (OverrideSim)
import qualified Lang.Crucible.Simulator as C
import           Lang.Crucible.Simulator.RegMap (RegValue)

-- crucible-llvm
import           Lang.Crucible.LLVM.Extension (ArchWidth)
import           Lang.Crucible.LLVM.MemModel (HasLLVMAnn, MemOptions)
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import           Lang.Crucible.LLVM.TypeContext (TypeContext)

import           Czz.SysTrace (SomeSysTrace(SomeSysTrace))
import qualified Czz.SysTrace as SysTrace

import qualified Czz.LLVM.Overrides.State.Env as State.Env

data Override arch e llargs llret
  = Override
    { genEffect ::
        forall wptr sym bak p ext rtp args ret.
        IsSymBackend sym bak =>
        HasLLVMAnn sym =>
        (wptr ~ ArchWidth arch) =>
        CLLVM.HasPtrWidth wptr =>
        (?lc :: TypeContext) =>
        (?memOpts :: MemOptions) =>
        bak ->
        C.GlobalVar CLLVM.Mem ->
        C.GlobalVar State.Env.EnvState ->
        Ctx.Assignment (C.RegEntry sym) llargs ->
        OverrideSim p sym ext rtp args ret e
      -- | Perform effect specified by 'genEffect'
    , doEffect ::
        forall wptr sym bak p ext rtp args ret.
        IsSymBackend sym bak =>
        HasLLVMAnn sym =>
        (wptr ~ ArchWidth arch) =>
        CLLVM.HasPtrWidth wptr =>
        (?lc :: TypeContext) =>
        (?memOpts :: MemOptions) =>
        bak ->
        e ->
        C.GlobalVar CLLVM.Mem ->
        C.GlobalVar State.Env.EnvState ->
        Ctx.Assignment (C.RegEntry sym) llargs ->
        OverrideSim p sym ext rtp args ret (RegValue sym llret)
    }

toLLVMOverride ::
  IsSymBackend sym bak =>
  HasLLVMAnn sym =>
  (wptr ~ ArchWidth arch) =>
  CLLVM.HasPtrWidth wptr =>
  (?lc :: TypeContext) =>
  (?memOpts :: MemOptions) =>
  bak ->
  IORef (SomeSysTrace eff) ->
  Lens.Prism' eff e ->
  Override arch e llargs llret ->
  C.GlobalVar CLLVM.Mem ->
  C.GlobalVar State.Env.EnvState ->
  Ctx.Assignment (C.RegEntry sym) llargs ->
  OverrideSim p sym ext rtp args ret (RegValue sym llret)
toLLVMOverride bak traceRef inj ov memVar envVar args = do
  Override gen act <- return ov
  SomeSysTrace trace <- liftIO (IORef.readIORef traceRef)
  case SysTrace.step trace of
    SysTrace.StepEnd trace' -> do
      e <- gen bak memVar envVar args
      ret <- act bak e memVar envVar args
      let trace'' = SysTrace.record (Lens.review inj e) trace'
      liftIO (IORef.writeIORef traceRef (SomeSysTrace trace''))
      return ret
    SysTrace.StepMid trace' eff -> do
      case Lens.preview inj eff of
        Nothing -> error "Mismatched call!"
        Just e -> do
          liftIO (IORef.writeIORef traceRef (SomeSysTrace trace'))
          act bak e memVar envVar args
