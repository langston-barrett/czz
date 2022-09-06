{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Czz.Fuzz.Type
  ( CzzPersonality(..)
  , FailedGoal(..)
  , Fuzzer(..)
  , SymbolicBits(..)
  )
where

import           Prelude hiding (log)

import           Data.IORef (IORef)
import qualified Data.Set as Set
import           Data.Sequence (Seq)
import           Data.Text (Text)
import           System.IO (Handle)

-- what4
import qualified What4.Interface as What4

-- crucible
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.FunctionHandle as C
import qualified Lang.Crucible.Simulator as C
import           Lang.Crucible.Types (UnitType)

import qualified Czz.Log as Log
import           Czz.Overrides (EffectTrace)
import           Czz.Record (Record)
import qualified Czz.Result as Res
import           Czz.Seed (Seed)
import           Czz.State (State)
import           Czz.SysTrace (Time(Begin))

data CzzPersonality = CzzPersonality

newtype FailedGoal sym
  = FailedGoal
    { getFailedGoal ::
        C.ProofGoal
          (C.CrucibleAssumptions (What4.SymExpr sym))
          (C.LabeledPred (What4.Pred sym) C.SimError)
    }

data SymbolicBits sym bak ext env eff fb
  = SymbolicBits
    { initState ::
        C.HandleAllocator ->
        IORef (EffectTrace eff) ->
        Seed 'Begin env eff ->
        IO (Handle, C.ExecState CzzPersonality sym ext (C.RegEntry sym UnitType))
    , explainResults ::
        forall ret.
        [FailedGoal sym] ->
        C.ExecResult CzzPersonality sym ext ret ->
        IO (Set.Set Res.Result)
    , getFeedback :: IO fb
    , instrumentation :: [C.GenericExecutionFeature sym]
    }

-- All operations should be thread-safe, should e.g. share no data by
-- non-atomically modifying an IORef.
data Fuzzer ext env eff k fb =
  Fuzzer
  { -- | 'Bool' is whether or not to mutate the last library call in the trace
    nextSeed ::
      Log.Has Text =>
      Seq (Record env eff k fb) ->
      IO (Seed 'Begin env eff, Bool)

  , onUpdate ::
      State env eff k fb ->
      IO ()

  -- Parts that may internally share state via IORefs, and use the symbolic
  -- backend.
  , symbolicBits ::
      forall sym bak.
      Log.Has Text =>
      C.IsSymBackend sym bak =>
      bak ->
      IO (SymbolicBits sym bak ext env eff fb)
  }
