{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Czz.LLVM
  ( fuzz
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
import qualified Czz.Coverage as Cover
import           Czz.Log (Logger)
import qualified Czz.Log as Log
import           Czz.KLimited (IsKLimited)
import qualified Czz.KLimited as KLimit
import           Czz.Fuzz (Fuzzer, FuzzError)
import qualified Czz.Fuzz as Fuzz
import qualified Czz.Random as Rand
import qualified Czz.Result as Res
import qualified Czz.Seed as Seed
import           Czz.State (State)

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
  Fuzzer LLVM Env Effect (Feedback k)
llvmFuzzer conf translation =
  Fuzz.Fuzzer
  { Fuzz.nextSeed = \records -> do
      -- TODO(lb): power schedule, mutation schedule
      record <- Rand.pickSeq records
      case record of
        Nothing -> return (Seed.begin Env.empty)
        Just r -> Mut.mutate r

  , Fuzz.symbolicBits = \bak -> do
      (sym :: sym) <- return (C.backendGetSym bak)

      coverageRef <- IORef.newIORef Cover.empty
      execFeat <- Cover.coverage (Just sym) coverageRef

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
        , Fuzz.instrumentation = [execFeat]
        , Fuzz.getFeedback = do
            -- TODO(lb): It would be amazing to have a generic monitoring/interception
            -- framework to capture these, so that it would be trivial to add new
            -- ones.
            readVars <- IORef.readIORef envVarRef
            opened <- IORef.readIORef openedRef
            cover <- Cover.bin <$> IORef.readIORef coverageRef
            let fb =
                  FB.Feedback
                  { FB.envVarsRead = Set.fromList readVars
                  , FB.filesOpened = Set.fromList opened
                  , FB.coverage = cover
                  }
            return (fb, FB.id fb)
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
              Log.log ?logger ("Missing implementation of " <> Text.pack nm)
              return (Res.MissingOverride (Text.pack nm))
            Just (callStack, badBehavior) -> do
              let _msg =
                    [ Text.pack (show (CLLVM.explainBB badBehavior))
                    , "At: " <> Text.pack (show (What4.plSourceLoc loc))
                    , Text.pack (show (CLLVM.ppBB badBehavior))
                    , "Callstack:"
                    , Text.pack (show (CLLVM.ppCallStack callStack))
                    ]
              -- Log.log ?logger (Text.unlines msg)
              return bug

-- | Library entry point
fuzz ::
  IsKLimited k =>
  Conf.Config ->
  Logger Text ->
  Logger Text ->
  IO (Either FuzzError (State Env Effect (Feedback k)))
fuzz conf stdoutLogger stderrLogger = do
  Log.log stdoutLogger ("Fuzzing program " <> Text.pack (Conf.prog conf))
  translation <- Trans.translate conf  -- Allowed to fail/call exit
  let fuzzer = llvmFuzzer conf translation
  Fuzz.fuzz (Conf.common conf) fuzzer stdoutLogger stderrLogger

-- | CLI entry point
main :: IO Exit.ExitCode
main = do
  conf <- CLI.cliConfig
  translation <- Trans.translate conf  -- Allowed to fail/call exit
  KLimit.withKLimit (CConf.pathLen (Conf.common conf)) $ do
    _finalState <- Fuzz.main (Conf.common conf) (llvmFuzzer conf translation)
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
