{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Czz.LLVM
  ( llvmFuzzer
  , fuzz
  , main
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
import qualified System.Exit as Exit
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

import qualified Czz.Config.Type as CConf
import           Czz.Log (Logger, Msg)
import qualified Czz.Log as Log
import           Czz.KLimited (IsKLimited)
import qualified Czz.KLimited as KLimit
import           Czz.Fuzz (Fuzzer, FuzzError)
import qualified Czz.Fuzz as Fuzz
import qualified Czz.Random as Rand
import qualified Czz.Record as Rec
import qualified Czz.Result as Res
import qualified Czz.Seed as Seed
import           Czz.State (State)
import           Czz.Stop (Stop)
import qualified Czz.Stop as Stop

import qualified Czz.LLVM.Config.CLI as CLI
import           Czz.LLVM.Config.Type (Config)
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
  Config ->
  Translation ->
  -- | Where to put simulator logs
  IO Handle ->
  Fuzzer LLVM Env Effect k Feedback
llvmFuzzer conf translation simLogs =
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
              (Conf.skip conf)
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
  Conf.Config ->
  Stop ->
  IO Handle ->
  Logger (Msg Text) ->
  Logger (Msg Text) ->
  IO (Either FuzzError (State Env Effect k Feedback))
fuzz conf stop simLogs stdoutLogger stderrLogger = do
  Log.with stdoutLogger $
    Log.info ("Fuzzing program " <> Text.pack (Conf.prog conf))
  translation <- Trans.translate conf  -- Allowed to fail/call exit
  let fuzzer = llvmFuzzer conf translation simLogs
  Fuzz.fuzz (Conf.common conf) stop fuzzer stdoutLogger stderrLogger

-- | CLI entry point
main :: IO Exit.ExitCode
main = do
  stop <- Stop.new
  conf <- CLI.cliConfig
  translation <- Trans.translate conf  -- Allowed to fail/call exit
  KLimit.withKLimit (CConf.pathLen (Conf.common conf)) $ do
    -- TODO(lb): non-void logger
    let simLog = Log.with Log.void Init.logToTempFile
    let fuzzer = llvmFuzzer conf translation simLog
    _finalState <- Fuzz.main (Conf.common conf) stop fuzzer
    return ()
  return Exit.ExitSuccess

-- TODO(lb):
-- printStats :: L.Module -> State -> IO ()
-- printStats llvmAst state = do
--   putStrLn "Stats:"
--   putStrLn ("Seeds: " ++ show (Seq.length (sPool state)))
--   putStrLn ("Blocks hit: " ++ show (Set.size (Set.unions (sCover state))))

--   let hit = Set.map Cover.getFnName (Set.unions (sCover state))
--   let defined = map ((\(L.Symbol s) -> s) . L.defName) (L.modDefines llvmAst)
--   let missed = Set.fromList defined `Set.difference` hit
--   putStrLn ("Functions hit (" ++ show (Set.size hit) ++ "):")
--   forM_ (Set.toList hit) $ \fn -> putStrLn ("- " ++ fn)
--   putStrLn ("Functions missed (" ++ show (Set.size missed) ++ "):")
--   forM_ (Set.toList missed) $ \fn -> putStrLn ("- " ++ fn)

--   putStrLn "Missing overrides:"
--   -- this miscounts....
--   let flat = mconcat . map Set.toList . Fold.toList
--   let ovs = Maybe.mapMaybe getOv (flat (fmap Rec.result (sPool state)))
--   forM_ (Map.toList (frequencies ovs)) $ \(nm, freq) ->
--     putStrLn ("  " ++ nm ++ ": " ++ show freq)
--   where
--     frequencies :: Ord a => [a] -> Map a Word
--     frequencies = foldr (\tag mp -> Map.insertWith (+) tag 1 mp) Map.empty

--     getOv =
--       \case
--         Res.MissingOverride ov -> Just ov
--         _ -> Nothing
