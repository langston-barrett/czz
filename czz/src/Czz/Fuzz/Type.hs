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
import           Czz.Record (Record)
import qualified Czz.Result as Res
import           Czz.Seed (Seed)
import           Czz.SysTrace (SomeSysTrace, Time(Begin))

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
        IORef (SomeSysTrace eff) ->
        Seed 'Begin env eff ->
        IO (Handle, C.ExecState CzzPersonality sym ext (C.RegEntry sym UnitType))
    , explainResults ::
        forall ret.
        [FailedGoal sym] ->
        C.ExecResult CzzPersonality sym ext ret ->
        IO (Set.Set Res.Result)
    , getFeedback :: IO (fb, Int)
    , instrumentation :: [C.GenericExecutionFeature sym]
    }

-- All operations should be thread-safe, should e.g. share no data by
-- non-atomically modifying an IORef.
data Fuzzer ext env eff fb =
  Fuzzer
  { -- Parts that don't change between runs, don't share state via IORef
    nextSeed ::
      Log.Has Text =>
      Seq (Record env eff fb) ->
      IO (Seed 'Begin env eff)

  -- Parts that may internally share state via IORefs, and use the symbolic
  -- backend.
  , symbolicBits ::
      forall sym bak.
      Log.Has Text =>
      C.IsSymBackend sym bak =>
      bak ->
      IO (SymbolicBits sym bak ext env eff fb)
  }
