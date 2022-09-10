{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Czz.JVM
  ( fuzz
  , main
  )
where

import qualified Control.Lens as Lens
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified System.Exit as Exit

-- crucible
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.Simulator.SimError as C

-- c-jvm
import           Lang.Crucible.JVM (JVM)
import qualified Lang.Crucible.JVM.Context as CJVM

import           Czz.Config.Type (FuzzConfig)
import qualified Czz.Config.Type as CConf
import           Czz.Log (Logger, Msg)
import           Czz.Fuzz (Fuzzer, FuzzError)
import qualified Czz.Fuzz as Fuzz
import           Czz.KLimited (IsKLimited)
import qualified Czz.KLimited as KLimit
import qualified Czz.Random as Rand
import qualified Czz.Record as Rec
import qualified Czz.Replay as Replay
import qualified Czz.Result as Res
import qualified Czz.Seed as Seed
import           Czz.State (State)
import           Czz.Stop (Stop)
import qualified Czz.Stop as Stop

import qualified Czz.JVM.Config.CLI as CLI
import           Czz.JVM.Config.Type (JVMConfig)
import qualified Czz.JVM.Config.Type as Conf
import qualified Czz.JVM.Init as Init
import           Czz.JVM.Translate (EntryPoint)
import qualified Czz.JVM.Translate as Trans

jvmFuzzer ::
  IsKLimited k =>
  JVMConfig ->
  CJVM.JVMContext ->
  EntryPoint ->
  Fuzzer JVM () () k ()
jvmFuzzer _jvmConf jvmCtx entryPoint =
  Fuzz.Fuzzer
  { Fuzz.nextSeed = \records -> do
      record <- Rand.pickSeq records
      case record of
        Nothing -> return (Seed.begin (), False)
        -- TODO(lb): mutate, power schedule, mutation schedule
        Just r -> return (Seed.rewind (Rec.seed r), False)

  , Fuzz.onUpdate = \_state -> return ()

  , Fuzz.symbolicBits = \bak -> do
      (_sym :: sym) <- return (C.backendGetSym bak)
      return $
        Fuzz.SymbolicBits
        { Fuzz.initState = \halloc effectRef seed -> do
            Init.initState
              bak
              jvmCtx
              entryPoint
              halloc
              effectRef
              seed
        , Fuzz.explainResults = \failedGoals _simResult -> do
            let explErr fGoal = do
                  let lPred = C.proofGoal (Fuzz.getFailedGoal fGoal)
                  let simError = lPred Lens.^. C.labeledPredMsg
                  let loc = C.simErrorLoc simError
                  return (Res.Bug loc (Text.pack (show (C.ppSimError simError))))
            goalExpls <- mapM explErr failedGoals
            if null failedGoals
              -- TODO(lb): is this correct? test e.g. with abort
              then return (Set.singleton Res.Ok)
              else return (Set.fromList goalExpls)
        , Fuzz.instrumentation = []
        , Fuzz.getFeedback = return ()
        }
  }

-- | Library entry point
fuzz ::
  IsKLimited k =>
  JVMConfig ->
  FuzzConfig ->
  Stop ->
  Logger (Msg Text) ->
  Logger (Msg Text) ->
  IO (Either FuzzError (State () () k ()))
fuzz jvmConf fuzzConf stop stdoutLogger stderrLogger = do
  (jvmCtx, entryPoint) <- Trans.translate jvmConf  -- Allowed to fail/call exit
  let fuzzer = jvmFuzzer jvmConf jvmCtx entryPoint
  Fuzz.fuzz fuzzConf stop fuzzer stdoutLogger stderrLogger

-- | CLI entry point
main :: IO Exit.ExitCode
main = do
  conf <- CLI.cliConfig
  let commonConf = Conf.common conf
  let baseConf = CConf.base commonConf
  let jvmConf = Conf.jvm conf
  case CConf.command commonConf of
    CConf.CmdFuzz fuzzConf -> do
      withFuzzer jvmConf (CConf.pathLen fuzzConf) $ \fuzzer -> do
        doFuzz baseConf fuzzConf fuzzer
    CConf.CmdReplay replayConf -> do
      KLimit.withKnownKLimit $ do
        (jvmCtx, entryPoint) <- Trans.translate jvmConf  -- Allowed to fail/call exit
        let fuzzer = jvmFuzzer jvmConf jvmCtx entryPoint
        _record <- Replay.replay (CConf.base commonConf) replayConf fuzzer
        return ()
  return Exit.ExitSuccess

  where
    withFuzzer ::
      JVMConfig ->
      Int ->
      (forall k. IsKLimited k => Fuzzer JVM () () k () -> IO a) ->
      IO a
    withFuzzer jvmConf kLimit k =
      KLimit.withKLimit kLimit $ do
        (jvmCtx, entryPoint) <- Trans.translate jvmConf  -- Allowed to fail/call exit
        k (jvmFuzzer jvmConf jvmCtx entryPoint)

    doFuzz baseConf fuzzConf fuzzer = do
      stop <- Stop.new
      _finalState <- Fuzz.main baseConf fuzzConf stop fuzzer
      return ()
