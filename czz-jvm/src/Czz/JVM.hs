{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Czz.JVM
  ( fuzz
  , main
  )
where

import qualified Control.Lens as Lens
import qualified Data.Hashable as Hash
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

import qualified Czz.Config.Type as CConf
import           Czz.Coverage (Coverage)
import qualified Czz.Coverage as Cover
import           Czz.Log (Logger)
import           Czz.Fuzz (Fuzzer, FuzzError)
import qualified Czz.Fuzz as Fuzz
import           Czz.KLimited (IsKLimited)
import qualified Czz.KLimited as KLimit
import qualified Czz.Random as Rand
import qualified Czz.Record as Rec
import qualified Czz.Result as Res
import qualified Czz.Seed as Seed
import           Czz.State (State)

import qualified Czz.JVM.Config.CLI as CLI
import           Czz.JVM.Config.Type (Config)
import qualified Czz.JVM.Config.Type as Conf
import qualified Czz.JVM.Init as Init
import           Czz.JVM.Translate (EntryPoint)
import qualified Czz.JVM.Translate as Trans

jvmFuzzer ::
  IsKLimited k =>
  Config ->
  CJVM.JVMContext ->
  EntryPoint ->
  Fuzzer JVM () () (Coverage k)
jvmFuzzer _conf jvmCtx entryPoint =
  Cover.withCoverage $
    Fuzz.Fuzzer
    { Fuzz.nextSeed = \records -> do
        record <- Rand.pickSeq records
        case record of
          Nothing -> return (Seed.begin ())
          -- TODO(lb): mutate, power schedule, mutation schedule
          Just r -> return (Seed.rewind (Rec.seed r))
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
          , Fuzz.getFeedback = return ((), Hash.hash ())
          }
    }

-- | Library entry point
fuzz ::
  IsKLimited k =>
  Conf.Config ->
  Logger Text ->
  Logger Text ->
  IO (Either FuzzError (State () () (Coverage k)))
fuzz conf stdoutLogger stderrLogger = do
  (jvmCtx, entryPoint) <- Trans.translate conf  -- Allowed to fail/call exit
  let fuzzer = jvmFuzzer conf jvmCtx entryPoint
  Fuzz.fuzz (Conf.common conf) fuzzer stdoutLogger stderrLogger

-- | CLI entry point
main :: IO Exit.ExitCode
main = do
  conf <- CLI.cliConfig
  (jvmCtx, entryPoint) <- Trans.translate conf  -- Allowed to fail/call exit
  KLimit.withKLimit (CConf.pathLen (Conf.common conf)) $ do
    let fuzzer = jvmFuzzer conf jvmCtx entryPoint
    _finalState <- Fuzz.main (Conf.common conf) fuzzer
    return ()
  return Exit.ExitSuccess
