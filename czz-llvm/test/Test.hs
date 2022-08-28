{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test (tests) where

import           Control.Category ((>>>))
import           Data.Functor.Contravariant ((>$<))
import qualified Data.List as List
import qualified Data.Text as Text
import qualified System.IO as IO

import qualified Test.Tasty as Tasty
import           Test.Tasty.ExpectedFailure (expectFail)
import qualified Test.Tasty.HUnit as TastyH
import qualified Test.Tasty.Golden as TastyG

import qualified Czz.Concurrent.Lock as Lock
import qualified Czz.Concurrent.Handle as Hand
import qualified Czz.Config.Type as CConf
import qualified Czz.Log as Log
import qualified Czz.Log.Concurrent as CLog
import qualified Czz.KLimited as KLimit
import qualified Czz.State as State
import qualified Czz.Stop as Stop

import qualified Czz.LLVM as Main
import qualified Czz.LLVM.Compile as Compile
import qualified Czz.LLVM.Config.Type as Conf
import qualified Czz.LLVM.Init as Init

import qualified Option as TestOpt

tests :: IO Tasty.TestTree
tests = do
  stop <- Stop.new

  bcFiles <-
    Compile.compileCFiles cFiles >>=
      (sequence >>>
        \case
          Left err -> do
            putStrLn "Failed to compile test program:"
            putStrLn (Compile.command err)
            putStrLn "stdout:"
            putStrLn (Compile.sout err)
            putStrLn "stderr:"
            putStrLn (Compile.serr err)
            error "Failed to compile test program"
          Right path -> return path)

  let conf =
        Conf.Config
          { Conf.common =
              CConf.Config
              { CConf.gas = Nothing
              , CConf.jobs = 1
              , CConf.pathLen = 1
              , CConf.seed = Nothing  -- Just 0
              , CConf.tries = Just 10
              , CConf.verbosity = Log.Error
              }
          , Conf.prog = "test.bc"
          , Conf.entryPoint = "main"
          , Conf.skip = []
          , Conf.onlyNeeded = True
          }

  -- TODO(lb): non-void logger
  let oneExec cf = cf { Conf.common = (Conf.common cf) { CConf.gas = Just 1 } }
  let checkOutput cf logger =
        let gold = replaceSuf (".bc" :: String) ".out" (Conf.prog cf)
            out = replaceSuf (".bc" :: String) ".czz.out" (Conf.prog cf)
            simLog = IO.openFile out IO.WriteMode
            maybeFail =
              if "fail" `List.isInfixOf` Conf.prog cf then expectFail else id
        in -- TODO(lb): expectFailBecause...?
           maybeFail $
             TastyG.goldenVsFile (Conf.prog cf <> " output") gold out $ do
               KLimit.withKLimit 1 $
                 Main.fuzz (oneExec cf) stop simLog logger logger >>=
                   \case
                     Left err -> error (show err)
                     Right _finalState -> return ()

  let simLog = Log.with Log.void Init.logToTempFile
  let assertFinalState cf logger f =
        TastyH.testCase (Conf.prog cf) $ do
          Main.fuzz cf stop simLog logger logger >>=
            \case
              Left err -> error (show err)
              Right finalState -> f finalState

  let expectNoBug cf logger prog =
        KLimit.withKLimit 1 $
          assertFinalState (cf { Conf.prog = cToBc prog }) logger $ \fs ->
              TastyH.assertBool
                ("Expected no bug in " ++ prog)
                (not (State.hasBug fs))

  let expectBug cf logger prog =
        KLimit.withKLimit 1 $
          assertFinalState (cf { Conf.prog = cToBc prog }) logger $ \fs ->
              TastyH.assertBool
                ("Expected bug in " ++ prog)
                (State.hasBug fs)

  stdStreams <- Lock.new Hand.stdStreams
  return $
    Tasty.askOption $ \verb ->
      let logger prog =
            if verb == TestOpt.All
            then
              fmap ((Text.pack prog <> ": ") <>) >$<
                CLog.logStdout Log.Debug stdStreams
            else Log.void
          bug prog = expectBug conf (logger prog) prog
          noBug prog = expectNoBug conf (logger prog) prog
          checkOut prog = checkOutput (conf { Conf.prog = prog }) (logger prog)
      in Tasty.testGroup "Tests"
           [ Tasty.testGroup "Bug tests"
               [ bug "assert-argc-eq-0.c"
               , bug "assert-argc-lt-0.c"
               , bug "getenv-deref.c"
               , noBug "assert-argc-geq-0.c"
               -- , noBug "argv00.c"
               , noBug "getenv-deref-2.c"  -- TODO(lb)
               , noBug "ret0-argv.c"
               , noBug "ret0-envp.c"
               , noBug "ret0-void.c"
               ]
           , Tasty.testGroup "Simulator fidelity (golden) tests" $
               map checkOut bcFiles
           ]

  where
    cFiles = "test/c"
    cToBc =
      ((cFiles ++ "/") ++) . replaceSuf (".c" :: String) (".bc" :: String)
    replaceSuf suf suf' =
      reverse . (reverse suf' ++) . drop (length suf) . reverse
