{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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
import           Control.Monad ((<=<), forM_, when)
import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Random as Random

import qualified System.Posix.Signals as Signal

-- crucible
import           Lang.Crucible.CFG.Extension (IsSyntaxExtension)

import qualified Czz.Coverage.Bucket.Bucketing as Bucketing
import qualified Czz.Concurrent.Lock as Lock
import qualified Czz.Concurrent.Handle as Hand
import qualified Czz.Config.Type as Conf
import           Czz.Fuzz.Type
import           Czz.Log (Logger, Msg)
import qualified Czz.Log as Log
import qualified Czz.Log.Concurrent as CLog
import           Czz.KLimited (IsKLimited)
import qualified Czz.Now as Now
import qualified Czz.Run as Run
import           Czz.State (State)
import qualified Czz.State as State
import qualified Czz.State.Stats as Stats
import           Czz.Stop (Stop)
import qualified Czz.Stop as Stop

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
  Conf.FuzzConfig ->
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
        (tid,) <$> Run.runWithZ3 doMut seed fuzzer

main ::
  Aeson.ToJSON env =>
  Aeson.ToJSON eff =>
  Aeson.ToJSON fb =>
  IsKLimited k =>
  IsSyntaxExtension ext =>
  Conf.BaseConfig ->
  Conf.FuzzConfig ->
  Stop ->
  Fuzzer ext env eff k fb ->
  IO (Either FuzzError (State env eff k fb))
main baseConf conf stop fuzzer = do
  let s = Conf.verbosity baseConf
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
