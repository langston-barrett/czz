{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Czz.LLVM.Fuzz
  ( llvmFuzzer
  , fuzz
  )
where

import           Data.ByteString (ByteString)
import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Control.Lens as Lens
import           System.IO (Handle)
import qualified System.Random as Random

-- what4
import qualified What4.Interface as What4
import qualified What4.ProgramLoc as What4

-- crucible
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.Simulator as C

-- crucible-llvm
import           Lang.Crucible.LLVM.Extension (LLVM)
import qualified Lang.Crucible.LLVM.Errors as CLLVM
import qualified Lang.Crucible.LLVM.Errors.MemoryError as CLLVM
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import qualified Lang.Crucible.LLVM.MemModel.CallStack as CLLVM
import qualified Lang.Crucible.LLVM.MemModel.Partial as CLLVM

import           Czz.Config.Type (FuzzConfig)
import           Czz.Log (Logger, Msg)
import qualified Czz.Log as Log
import           Czz.KLimited (IsKLimited)
import           Czz.Fuzz (Fuzzer, FuzzError)
import qualified Czz.Fuzz as Fuzz
import qualified Czz.Random as Rand
import qualified Czz.Record as Rec
import qualified Czz.Result as Res
import qualified Czz.Seed as Seed
import           Czz.State (State)
import           Czz.Stop (Stop)

import           Czz.LLVM.Config.Type (LLVMConfig)
import qualified Czz.LLVM.Config.Type as Conf
import           Czz.LLVM.Env (Env)
import qualified Czz.LLVM.Env as Env
import           Czz.LLVM.Feedback (Feedback)
import qualified Czz.LLVM.Feedback as FB
import qualified Czz.LLVM.Init as Init
import qualified Czz.LLVM.Mutate as Mut
import           Czz.LLVM.Overrides (Effect)
import           Czz.LLVM.Translate (Translation)
import qualified Czz.LLVM.Translate as Trans

-- TODO(lb): Mid-term
--
-- - Logging
-- - Benchmarks
-- - More overrides

llvmFuzzer ::
  IsKLimited k =>
  LLVMConfig ->
  Translation ->
  -- | Where to put simulator logs
  IO Handle ->
  Init.ExtraInit ->
  Fuzzer LLVM Env Effect k Feedback
llvmFuzzer llvmConf translation simLogs extraInit =
  Fuzz.Fuzzer
  { Fuzz.nextSeed = \records -> do
      -- TODO(lb): power schedule, mutation schedule
      record <- Rand.pickSeq records
      case record of
        Nothing -> return (Seed.begin Env.empty, False)
        Just r -> do
          doMutTrace <- Random.randomIO :: IO Bool
          if doMutTrace
            then return (Seed.rewind (Rec.seed r), True)
            else (, False) <$> Mut.mutate r

  , Fuzz.onUpdate = \_state -> return ()

  , Fuzz.symbolicBits = \bak -> do
      (sym :: sym) <- return (C.backendGetSym bak)

      envVarRef <- IORef.newIORef ([] :: [ByteString])
      openedRef <- IORef.newIORef ([] :: [ByteString])
      bbMapRef <- IORef.newIORef Map.empty :: IO (IORef (CLLVM.LLVMAnnMap sym))
      let ?recordLLVMAnnotation =
            \callStack an bb ->
              IORef.modifyIORef bbMapRef (Map.insert an (callStack, bb))
      return $
        Fuzz.SymbolicBits
        { Fuzz.initState = \halloc effectRef seed -> do
            let ?memOpts = CLLVM.defaultMemOptions
            Init.initState
              (Just (C.backendGetSym bak))
              bak
              halloc
              translation
              simLogs
              envVarRef
              openedRef
              effectRef
              seed
              (Conf.skip llvmConf)
              extraInit
        , Fuzz.explainResults = \failedGoals _simResult -> do
            bbMap <- IORef.readIORef bbMapRef
            goalExpls <- mapM (explainFailedGoal sym bbMap) failedGoals
            if null failedGoals
              -- TODO(lb): is this correct? test e.g. with abort
              then return (Set.singleton Res.Ok)
              else return (Set.fromList goalExpls)
        , Fuzz.instrumentation = []
        , Fuzz.getFeedback = do
            -- TODO(lb): It would be amazing to have a generic monitoring/interception
            -- framework to capture these, so that it would be trivial to add new
            -- ones.
            readVars <- IORef.readIORef envVarRef
            opened <- IORef.readIORef openedRef
            let fb =
                  FB.Feedback
                  { FB.envVarsRead = Set.fromList readVars
                  , FB.filesOpened = Set.fromList opened
                  }
            return fb
        }
  }
  where
    explainFailedGoal ::
      Log.Has Text =>
      C.IsSymInterface sym =>
      sym ->
      CLLVM.LLVMAnnMap sym ->
      Fuzz.FailedGoal sym ->
      IO Res.Result
    explainFailedGoal sym bbMap failedGoal = do
      let lPred = C.proofGoal (Fuzz.getFailedGoal failedGoal)
      let simError = lPred Lens.^. C.labeledPredMsg
      let loc = C.simErrorLoc simError
      let bug = Res.Bug loc (Text.pack (show (C.ppSimError simError)))
      case What4.getAnnotation sym (lPred Lens.^. C.labeledPred) of
        Nothing -> return bug
        Just ann ->
          case Map.lookup (CLLVM.BoolAnn ann) bbMap of
            Nothing -> return bug
            Just (_cs, CLLVM.BBMemoryError (CLLVM.MemoryError (CLLVM.MemLoadHandleOp _ (Just nm) _ _) (CLLVM.BadFunctionPointer CLLVM.NoOverride))) -> do
              Log.debug ("Missing implementation of " <> Text.pack nm)
              return (Res.MissingOverride (Text.pack nm))
            Just (callStack, badBehavior) -> do
              let msg =
                    [ Text.pack (show (CLLVM.explainBB badBehavior))
                    , "At: " <> Text.pack (show (What4.plSourceLoc loc))
                    , Text.pack (show (CLLVM.ppBB badBehavior))
                    , "Callstack:"
                    , Text.pack (show (CLLVM.ppCallStack callStack))
                    ]
              Log.debug (Text.unlines msg)
              return bug

-- | Library entry point
fuzz ::
  IsKLimited k =>
  LLVMConfig ->
  FuzzConfig ->
  Stop ->
  IO Handle ->
  Logger (Msg Text) ->
  Logger (Msg Text) ->
  IO (Either FuzzError (State Env Effect k Feedback))
fuzz llvmConf fuzzConf stop simLogs stdoutLogger stderrLogger = do
  Log.with stdoutLogger $
    Log.info ("Fuzzing program " <> Text.pack (Conf.prog llvmConf))
  translation <- Trans.translate llvmConf  -- Allowed to fail/call exit
  let fuzzer = llvmFuzzer llvmConf translation simLogs Init.noExtraInit
  Fuzz.fuzz fuzzConf stop fuzzer stdoutLogger stderrLogger
