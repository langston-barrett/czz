{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Czz.Overrides
  ( EffectTrace
  , makeEffectTrace
  , extract
  , Override(..)
  , toOverride
  , toOverride'
  )
where

import           Prelude hiding (id)
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

import           Czz.SysTrace (SysTrace, SomeSysTrace(SomeSysTrace), Time(End))
import qualified Czz.SysTrace as SysTrace

data EffectTrace eff
  = forall t.
    EffectTrace
    { _trace :: SysTrace t eff
    , _modifyLast :: Bool
    }

makeEffectTrace :: SysTrace t eff -> Bool -> EffectTrace eff
makeEffectTrace = EffectTrace

-- TODO(lb): assert already at end
extract :: EffectTrace eff -> SysTrace 'End eff
extract (EffectTrace trace _) = SysTrace.fastForward trace

data Override sym bak e ovargs ovret
  = Override
    { genEffect ::
        forall p ext rtp args ret.
        -- Possible effect to mutate
        Maybe e ->
        Ctx.Assignment (C.RegEntry sym) ovargs ->
        OverrideSim p sym ext rtp args ret e
      -- | Perform effect specified by 'genEffect'
    , doEffect ::
        forall p ext rtp args ret.
        e ->
        Ctx.Assignment (C.RegEntry sym) ovargs ->
        OverrideSim p sym ext rtp args ret (RegValue sym ovret)
    }

toOverride ::
  IsSymBackend sym bak =>
  bak ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff e ->
  Override sym bak e ovargs ovret ->
  Ctx.Assignment (C.RegEntry sym) ovargs ->
  OverrideSim p sym ext rtp args ret (RegValue sym ovret)
toOverride _bak traceRef inj ov args = do
  Override gen act <- return ov
  EffectTrace trace modLast <- liftIO (IORef.readIORef traceRef)
  case SysTrace.step trace of
    SysTrace.StepEnd trace' -> do
      -- Execution has passed the end of the trace, generate new responses to
      -- library calls.
      e <- gen Nothing args
      ret <- act e args
      let trace'' = SysTrace.record (Lens.review inj e) trace'
      liftIO (IORef.writeIORef traceRef (EffectTrace trace'' modLast))
      return ret
    SysTrace.StepMid trace' eff ->
      -- Executions is proceeding along the trace.
      case Lens.preview inj eff of
        Nothing -> error "Mismatched call!"
        Just e -> do
          (eff', SomeSysTrace trace'') <-
            if not modLast
            then return (e, SomeSysTrace trace')
            else
              case SysTrace.step trace' of
                SysTrace.StepMid {} -> return (e, SomeSysTrace trace')
                SysTrace.StepEnd endTrace -> do
                  -- Execution has reached the last library call in the trace,
                  -- and mutation of the response was requested. Mutate the
                  -- response and add it to the end of the trace.
                  e' <- gen (Just e) args
                  let tr = SysTrace.snocEnd endTrace (Lens.review inj e)
                  return (e', SomeSysTrace tr)

          liftIO (IORef.writeIORef traceRef (EffectTrace trace'' modLast))
          act eff' args

-- | Just for convenient TypeApplications to avoid extra type signatures
toOverride' ::
  forall ovargs ovret p sym bak ext rtp args ret eff e.
  IsSymBackend sym bak =>
  bak ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff e ->
  Ctx.Assignment (C.RegEntry sym) ovargs ->
  Override sym bak e ovargs ovret ->
  OverrideSim p sym ext rtp args ret (RegValue sym ovret)
toOverride' bak traceRef inj args ov = toOverride bak traceRef inj ov args
