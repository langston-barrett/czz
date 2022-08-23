{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Data.Either as Either
import           Control.Exception.Base (SomeException)
import           Data.Functor ((<&>))
import qualified Data.IORef as IORef
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Control.Lens as Lens
import           Control.Monad ((<=<), forM_, forM, unless)
import qualified System.Random as Random
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

import qualified Czz.Concurrent.Lock as Lock
import qualified Czz.Concurrent.Handle as Hand
import qualified Czz.Config.Type as Conf
import           Czz.Fuzz.Type
import           Czz.Log (Logger, Msg)
import qualified Czz.Log as Log
import qualified Czz.Log.Concurrent as CLog
import           Czz.Record (Record)
import qualified Czz.Record as Rec
import           Czz.Seed (Seed)
import qualified Czz.Seed as Seed
import           Czz.State (State)
import qualified Czz.State as State
import           Czz.SysTrace (SomeSysTrace(SomeSysTrace), Time(Begin))
import qualified Czz.SysTrace as SysTrace

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
  forall ext env eff fb sym bak s st fs solver.
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
  Seed 'Begin env eff ->
  Fuzzer ext env eff fb ->
  IO (Record env eff fb)
run _conf bak halloc seed fuzzer = do
  (sym :: sym) <- return (C.backendGetSym bak)
  -- Stuff that gets recorded during execution
  effectRef <-
    IORef.newIORef (SomeSysTrace (Seed.effects seed))
      :: IO (IORef.IORef (SomeSysTrace eff))
  frame <- C.pushAssumptionFrame bak
  -- TODO(lb): something with result, probably
  -- TODO(lb): allow other execution features
  symbBits <- symbolicBits fuzzer bak
  (simLogHandle, initSt) <- initState symbBits halloc effectRef seed
  let execFeats =
        map C.genericToExecutionFeature (instrumentation symbBits)
  simResult <- C.executeCrucible execFeats initSt
  IO.hClose simLogHandle
  () <-
    case simResult of
      C.AbortedResult _ ar -> do
        Log.debug "Aborted!"
        Log.debug $
          case ar of
            C.AbortedExec _reason _ -> "" -- show (C.ppAbortExecReason reason)
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

  results <- explainResults symbBits (map FailedGoal failedGoals) simResult

  -- TODO(lb): maybe assert at end of trace?
  SomeSysTrace effects_ <- IORef.readIORef effectRef
  let effects = SysTrace.fastForward effects_

  (fb, fbId) <- getFeedback symbBits

  return $
    Rec.Record
      { Rec.seed = Seed.record seed effects
      , Rec.result = results
      , Rec.feedback = fb
      , Rec.feedbackId = fbId
      }

newtype FuzzError
  = ThreadError SomeException
  deriving Show

-- | Main entry point.
--
-- TODO(lb): no quitting, configurable logging
-- TODO(lb): notion of crashes, deduplication of crashes
fuzz ::
  IsSyntaxExtension ext =>
  Conf.Config ->
  Fuzzer ext env eff fb ->
  Logger (Msg Text) ->
  Logger (Msg Text) ->
  IO (Either FuzzError (State env eff fb))
fuzz conf fuzzer stdoutLogger stderrLogger = do
  initRandom (Conf.seed conf)
  runResultVar <- MVar.newEmptyMVar
  initialState <- State.newIO
  Log.with stdoutLogger (go runResultVar 0 initialState)
  where
    initRandom =
      Random.setStdGen <=<
        \case
          Nothing -> Random.initStdGen
          Just seed -> return (Random.mkStdGen seed)


    tooManyTries state =
      case Conf.tries conf of
        Nothing -> False
        Just maxTries -> State.tries state > maxTries

    go runResultVar running state = do
      if tooManyTries state
      then do
        Log.with stdoutLogger $
          Log.info "Too many tries without new coverage! Giving up."
        return (Right state)
      else do
        if running >= Conf.jobs conf
          then blockOnResult runResultVar state running
          else do
            _threadId <- newThread runResultVar state
            go runResultVar (running + 1) state

    blockOnResult runResultVar state running =
      MVar.takeMVar runResultVar >>=
        \case
          Left err -> do
            Log.with stderrLogger $
              Log.error ("Thread exited with error! " <> Text.pack (show err))
            return (Left (ThreadError err))
          Right record -> do
            (new, state') <- State.record record state
            Log.with stdoutLogger $
              if new
                then Log.info ("New coverage :)" :: Text)
                else Log.info ("No new coverage :(" :: Text)
            go runResultVar (running - 1) state'

    -- TODO(lb): is forkFinally enough to handle all exceptions?
    newThread runResultVar state =
      flip Con.forkFinally (MVar.putMVar runResultVar) $ do
        logger <-
          if Conf.jobs conf > 1
          then CLog.pfxThreadId stdoutLogger
          else return stdoutLogger
        let ?logger = logger
        seed <- nextSeed fuzzer (State.pool state)
        withZ3 $ \bak -> do
          halloc <- C.newHandleAllocator
          run conf bak halloc seed fuzzer

main ::
  IsSyntaxExtension ext =>
  Conf.Config ->
  Fuzzer ext env eff fb ->
  IO (Either FuzzError (State env eff fb))
main conf fuzzer = do
  let s = Conf.verbosity conf
  let cap = 4096 -- TODO(lb): Good default? Configurable?
  lss <- Lock.new Hand.stdStreams
  (_tid, stdoutLogger) <- CLog.forkStdoutLogger s lss cap
  (_tid, stderrLogger) <- CLog.forkStderrLogger s lss cap
  fuzz conf fuzzer stdoutLogger stderrLogger

-- TODO(lb)
-- setSimulatorVerbosity :: (W4.IsSymExprBuilder sym) => Int -> sym -> IO ()
-- setSimulatorVerbosity verbosity sym = do
--   verbSetting <- W4.getOptionSetting W4.verbosity (W4.getConfiguration sym)
--   _ <- W4.setOpt verbSetting (toInteger verbosity)
--   return ()
