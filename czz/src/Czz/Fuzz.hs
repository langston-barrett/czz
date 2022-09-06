{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Czz.Fuzz
  ( module Czz.Fuzz.Type
  , FuzzError
  , fuzz
  , main
  )
where

import           Prelude hiding (log)

import qualified Control.Concurrent as Con
import qualified Control.Concurrent.MVar as MVar
import           Control.Exception.Base (SomeException)
import qualified Data.Aeson as Aeson
import qualified Data.Either as Either
import           Data.Functor ((<&>))
import qualified Data.IORef as IORef
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Control.Lens as Lens
import           Control.Monad ((<=<), forM_, forM, when, unless)
import qualified System.Random as Random
import qualified System.IO as IO

import qualified System.Posix.Signals as Signal

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

import qualified Czz.Coverage.Bucket.Bucketing as Bucketing
import qualified Czz.Concurrent.Lock as Lock
import qualified Czz.Concurrent.Handle as Hand
import qualified Czz.Config.Type as Conf
import qualified Czz.Coverage.Feature as CFeat
import qualified Czz.Coverage.Seed as CSeed
import           Czz.Fuzz.Type
import           Czz.Log (Logger, Msg)
import qualified Czz.Log as Log
import qualified Czz.Log.Concurrent as CLog
import           Czz.KLimited (IsKLimited)
import qualified Czz.Now as Now
import           Czz.Overrides (EffectTrace)
import qualified Czz.Overrides as Ov
import           Czz.Record (Record)
import qualified Czz.Record as Rec
import           Czz.Seed (Seed)
import qualified Czz.Seed as Seed
import           Czz.State (State)
import qualified Czz.State as State
import qualified Czz.State.Stats as Stats
import           Czz.Stop (Stop)
import qualified Czz.Stop as Stop
import           Czz.SysTrace (Time(Begin))

-- | Not exported.
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
  sym :: What4.ExprBuilder t st (What4.Flags fm) <-
    What4.newExprBuilder What4.FloatIEEERepr What4.EmptyExprBuilderState nonceGen
  What4.extendConfig What4.z3Options (What4.getConfiguration sym)
  C.withZ3OnlineBackend sym C.NoUnsatFeatures What4.noFeatures k

-- | Execute the program once.
--
-- Helper for 'loop', not exported
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
  Conf.Config ->
  bak ->
  C.HandleAllocator ->
  -- | Mutate last library call response?
  Bool ->
  Seed 'Begin env eff ->
  Fuzzer ext env eff k fb ->
  IO (Record env eff k fb)
run _conf bak halloc doMut seed fuzzer = do
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

newtype FuzzError
  = ThreadError SomeException
  deriving Show

-- | Main entry point.
--
-- TODO(lb): notion of crashes, deduplication of crashes
fuzz ::
  Aeson.ToJSON env =>
  Aeson.ToJSON eff =>
  Aeson.ToJSON fb =>
  IsKLimited k =>
  IsSyntaxExtension ext =>
  Conf.Config ->
  Stop ->
  Fuzzer ext env eff k fb ->
  Logger (Msg Text) ->
  Logger (Msg Text) ->
  IO (Either FuzzError (State env eff k fb))
fuzz conf stop fuzzer stdoutLogger stderrLogger = do
  initRandom (Conf.seed conf)
  runResultVar <- MVar.newEmptyMVar
  initialState <- State.newIO
  catchSigint
  Log.with stdoutLogger $ do
    go runResultVar Set.empty initialState
  where
    bucketing = Bucketing.fromName (Conf.bucketing conf)
    
    initRandom =
      Random.setStdGen <=<
        \case
          Nothing -> Random.initStdGen
          Just seed -> return (Random.mkStdGen seed)

    tooLittleGas state =
      case Conf.gas conf of
        Nothing -> False
        Just maxExecs ->
          Stats.execs (State.stats state) >= fromIntegral maxExecs

    tooManyTries state =
      case Conf.tries conf of
        Nothing -> False
        Just maxTries -> State.tries state > fromIntegral maxTries

    catchSigint = do
      let handle = do
            putStrLn "Handling SIGINT..."
            Stop.send stop
      _oldHandler <-
        Signal.installHandler Signal.keyboardSignal (Signal.Catch handle) Nothing
      return ()

    go runResultVar running state = do
      shouldStop <- maybeStop runResultVar running state
      if shouldStop
      then return (Right state)
      else do
        if Set.size running >= Conf.jobs conf
          then blockOnResult runResultVar state running
          else do
            threadId <- newThread runResultVar state
            go runResultVar (Set.insert threadId running) state

    maybeStop runResultVar running state = do
      interrupted <- Stop.should stop
      let shouldStop = interrupted || tooManyTries state || tooLittleGas state
      when shouldStop $
        Log.with stdoutLogger $ do
          if interrupted
            then Log.warn "Interrupted! Waiting for threads to finish..."
            else
              if tooManyTries state
              then Log.info "Too many tries without new coverage! Giving up."
              else Log.info "Out of gas."
          now <- Now.now
          -- TODO(lb): How to shut down stdout/stderr?
          --
          -- Current approach (blocking exceptions) works OK, but messages still
          -- seem to get dropped?
          let rn = Set.size running
          forM_ (zip [1..] (Set.toList running)) $ \(i :: Int, _tid) -> do
            Log.info (Text.pack ("Waiting for thread " ++ show i ++ " / " ++ show rn))
            _result <- MVar.takeMVar runResultVar
            return ()
          let stats = State.stats state
          Log.info "Stats:"
          Log.info ("execs: " <> Text.pack (show (Stats.execs stats)))
          Log.info ("execs/sec: " <> Text.pack (show (Stats.execsPerSec stats now)))
          Log.info ("pool: " <> Text.pack (show (Stats.poolSize stats)))
          Log.info ("missing:\n" <> Text.unlines (Set.toList (Stats.missing stats)))
      return shouldStop

    whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
    whenJust = forM_

    blockOnResult runResultVar state running =
      MVar.takeMVar runResultVar >>=
        \case
          Left err -> do
            Log.with stderrLogger $
              Log.error ("Thread exited with error! " <> Text.pack (show err))
            return (Left (ThreadError err))
          Right (tid, record) -> do
            (_newFeedback, state') <- State.record bucketing record state
            whenJust (Conf.stateDir conf) $ \dir -> State.serialize dir state'
            onUpdate fuzzer state'
            -- Log.with stdoutLogger $
            --   if new
            --     then Log.info ("New coverage :)" :: Text)
            --     else Log.info ("No new coverage :(" :: Text)
            go runResultVar (Set.delete tid running) state'

    -- TODO(lb): is forkFinally enough to handle all exceptions?
    newThread runResultVar state =
      flip Con.forkFinally (MVar.putMVar runResultVar) $ do
        catchSigint
        tid <- Con.myThreadId
        logger <-
          if Conf.jobs conf > 1
          then CLog.pfxThreadId stdoutLogger
          else return stdoutLogger
        let ?logger = logger
        (seed, doMut) <- nextSeed fuzzer (State.pool state)
        withZ3 $ \bak -> do
          halloc <- C.newHandleAllocator
          (tid,) <$> run conf bak halloc doMut seed fuzzer

main ::
  Aeson.ToJSON env =>
  Aeson.ToJSON eff =>
  Aeson.ToJSON fb =>
  IsKLimited k =>
  IsSyntaxExtension ext =>
  Conf.Config ->
  Stop ->
  Fuzzer ext env eff k fb ->
  IO (Either FuzzError (State env eff k fb))
main conf stop fuzzer = do
  let s = Conf.verbosity conf
  let cap = 4096 -- TODO(lb): Good default? Configurable?
  lss <- Lock.new Hand.stdStreams
  CLog.withStdoutLogger s lss cap $ \(_tid, stdoutLogger) ->
    CLog.withStderrLogger s lss cap $ \(_tid, stderrLogger) -> do
      fuzz conf stop fuzzer stdoutLogger stderrLogger

-- TODO(lb)
-- setSimulatorVerbosity :: (W4.IsSymExprBuilder sym) => Int -> sym -> IO ()
-- setSimulatorVerbosity verbosity sym = do
--   verbSetting <- W4.getOptionSetting W4.verbosity (W4.getConfiguration sym)
--   _ <- W4.setOpt verbSetting (toInteger verbosity)
--   return ()
