{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Czz.Overrides
  ( EffectTrace
  , makeEffectTrace
  , extract
  , AnyOverrideSim(..)
  , Override(..)
  , toOverride
  )
where

import           Prelude hiding (id)
import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import           Data.Proxy (Proxy(Proxy))

-- p-u
import qualified Data.Parameterized.Context as Ctx

-- crucible
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

-- | Needed to avoid impredicative types
newtype AnyOverrideSim sym ext a
  = AnyOverrideSim
      { getAnyOverrideSym ::
          forall p rtp args ret. OverrideSim p sym ext rtp args ret a
      }

data Override sym bak e ovargs ovret
  = Override
    { genEffect ::
        forall proxy ext.
        proxy ext ->
        -- Possible effect to mutate
        Maybe e ->
        Ctx.CurryAssignment
          ovargs
          (C.RegEntry sym)
          (AnyOverrideSim sym ext e)
      -- | Perform effect specified by 'genEffect'
    , doEffect ::
        forall proxy ext.
        proxy ext ->
        e ->
        Ctx.CurryAssignment
          ovargs
          (C.RegEntry sym)
          (AnyOverrideSim sym ext (RegValue sym ovret))
    }

toOverride ::
  -- Type variables are in this order for convenient TypeApplications
  forall ovret ovargs p sym bak ext rtp args ret eff e.
  Ctx.CurryAssignmentClass ovargs =>
  IORef (EffectTrace eff) ->
  Lens.Prism' eff e ->
  Ctx.Assignment (C.RegEntry sym) ovargs ->
  Override sym bak e ovargs ovret ->
  OverrideSim p sym ext rtp args ret (RegValue sym ovret)
toOverride traceRef inj args ov = do
  Override gen act <- return ov
  EffectTrace trace modLast <- liftIO (IORef.readIORef traceRef)
  let proxy = Proxy @ext
  case SysTrace.step trace of
    SysTrace.StepEnd trace' -> do
      e <- getAnyOverrideSym (Ctx.uncurryAssignment (gen proxy Nothing) args)
      ret <- getAnyOverrideSym (Ctx.uncurryAssignment (act proxy e) args)
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
                  e' <- getAnyOverrideSym (Ctx.uncurryAssignment (gen proxy (Just e)) args)
                  let tr = SysTrace.snocEnd endTrace (Lens.review inj e)
                  return (e', SomeSysTrace tr)

          liftIO (IORef.writeIORef traceRef (EffectTrace trace'' modLast))
          getAnyOverrideSym (Ctx.uncurryAssignment (act proxy eff') args)
