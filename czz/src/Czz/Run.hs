{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Czz.Run
  ( run
  , withZ3
  , runWithZ3
  )
where

import           Prelude hiding (log)

import qualified Data.Either as Either
import           Data.Functor ((<&>))
import qualified Data.IORef as IORef
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Control.Lens as Lens
import           Control.Monad (forM_, forM, unless)
import qualified System.IO as IO

-- p-u
import qualified Data.Parameterized.Nonce as Nonce
import           Data.Parameterized.Some (Some(..))

-- what4
import qualified What4.Config as What4
import qualified What4.Expr as What4
import qualified What4.Interface as What4
import qualified What4.ProblemFeatures as What4
import qualified What4.Protocol.Online as What4
import qualified What4.Protocol.SMTWriter as What4
import qualified What4.Solver as What4

-- crucible
import           Lang.Crucible.CFG.Extension (IsSyntaxExtension)
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.Backend.Online as C
import qualified Lang.Crucible.FunctionHandle as C
import qualified Lang.Crucible.Simulator as C

import qualified Czz.Coverage.Feature as CFeat
import qualified Czz.Coverage.Seed as CSeed
import           Czz.Fuzz.Type
import qualified Czz.Log as Log
import           Czz.KLimited (IsKLimited)
import           Czz.Overrides (EffectTrace)
import qualified Czz.Overrides as Ov
import           Czz.Record (Record)
import qualified Czz.Record as Rec
import           Czz.Seed (Seed)
import qualified Czz.Seed as Seed
import           Czz.SysTrace (Time(Begin))

-- | Execute the program once.
--
-- Used both for fuzzing and replaying seeds.
run ::
  forall ext env eff k fb sym bak s st fs solver.
  IsKLimited k =>
  Log.Has Text =>
  IsSyntaxExtension ext =>
  C.IsSymBackend sym bak =>
  (sym ~ What4.ExprBuilder s st fs) =>
  (bak ~ C.OnlineBackend solver s st fs) =>
  What4.OnlineSolver solver =>
  --
  bak ->
  C.HandleAllocator ->
  -- | Mutate last library call response?
  Bool ->
  Seed 'Begin env eff ->
  Fuzzer ext env eff k fb ->
  IO (Record env eff k fb)
run bak halloc doMut seed fuzzer = do
  (sym :: sym) <- return (C.backendGetSym bak)
  -- Stuff that gets recorded during execution
  coverageRef <- IORef.newIORef CSeed.empty
  effectRef <-
    IORef.newIORef (Ov.makeEffectTrace (Seed.effects seed) doMut)
      :: IO (IORef.IORef (EffectTrace eff))
  frame <- C.pushAssumptionFrame bak
  -- TODO(lb): something with result, probably
  symbBits <- symbolicBits fuzzer bak
  (simLogHandle, initSt) <- initState symbBits halloc effectRef seed
  coverFeat <- CFeat.coverage (Just sym) coverageRef
  let execFeats =
        map C.genericToExecutionFeature (coverFeat:instrumentation symbBits)
  simResult <- C.executeCrucible execFeats initSt
  IO.hClose simLogHandle
  () <-
    case simResult of
      C.AbortedResult _ ar -> do
        Log.debug "Aborted!"
        Log.debug $
          case ar of
            C.AbortedExec reason _ -> Text.pack (show (C.ppAbortExecReason reason))
            C.AbortedExit exitCode -> Text.pack (show exitCode)
            C.AbortedBranch{} -> "branch..."
      C.TimeoutResult{} -> Log.debug "Timeout!"
      C.FinishedResult{} -> Log.debug "Finished!"
  C.popUntilAssumptionFrame bak frame
  -- ??? Needed?
  enableOpt <-
    What4.getOptionSetting C.enableOnlineBackend (What4.getConfiguration sym)
  _ <- What4.setOpt enableOpt True

  goals <- C.getProofObligations bak
  let goals' = maybe [] C.goalsToList goals
  goalResults <-
    C.withSolverProcess bak (error "panic") $ \sp ->
      What4.inNewFrame sp $ do
        forM goals' $ \goal -> do
          let conn = What4.solverConn sp
          assumps <- C.flattenAssumptions sym (C.proofAssumptions goal)
          forM_ assumps $ \assump ->
            unless (C.trivialAssumption assump) $
              do What4.assumeFormula conn =<<
                    What4.mkFormula conn (C.assumptionPred assump)
          What4.assumeFormula conn =<<
            What4.mkFormula conn =<<
              What4.notPred sym (C.proofGoal goal Lens.^. C.labeledPred)
          What4.check sp "proof" <&>
            \case
              What4.Unsat () -> Right goal
              What4.Sat () -> Left goal
              What4.Unknown -> Left goal
  let (failedGoals, _provedGoals) = Either.partitionEithers goalResults

  coverage <- IORef.readIORef coverageRef
  results <- explainResults symbBits (map FailedGoal failedGoals) simResult
  effects <- Ov.extract <$> IORef.readIORef effectRef
  fb <- getFeedback symbBits

  return $
    Rec.Record
      { Rec.coverage = coverage
      , Rec.seed = Seed.record seed effects
      , Rec.result = results
      , Rec.feedback = fb
      }

withZ3 ::
  (forall sym bak solver s st fs.
   C.IsSymBackend sym bak =>
   What4.OnlineSolver solver =>
   (sym ~ What4.ExprBuilder s st fs) =>
   (bak ~ C.OnlineBackend solver s st fs) =>
   bak ->
   IO a) ->
  IO a
withZ3 k = do
  Some (nonceGen :: Nonce.NonceGenerator IO s) <- Nonce.newIONonceGenerator
  sym :: What4.ExprBuilder s st (What4.Flags fm) <-
    What4.newExprBuilder What4.FloatIEEERepr What4.EmptyExprBuilderState nonceGen
  What4.extendConfig What4.z3Options (What4.getConfiguration sym)
  C.withZ3OnlineBackend sym C.NoUnsatFeatures What4.noFeatures k

-- | Execute 'run' with Z3 and a fresh handle allocator.
runWithZ3 ::
  forall ext env eff k fb.
  IsKLimited k =>
  Log.Has Text =>
  IsSyntaxExtension ext =>
  -- | Mutate last library call response?
  Bool ->
  Seed 'Begin env eff ->
  Fuzzer ext env eff k fb ->
  IO (Record env eff k fb)
runWithZ3 doMut seed fuzzer = do
  withZ3 $ \bak -> do
    halloc <- C.newHandleAllocator
    run bak halloc doMut seed fuzzer
