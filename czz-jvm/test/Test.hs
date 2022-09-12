{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test (tests) where

import           Data.Functor.Contravariant ((>$<))
import qualified Data.Text as Text

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as TastyH

import qualified Czz.Concurrent.Lock as Lock
import qualified Czz.Concurrent.Handle as Hand
import qualified Czz.Config.Type as CConf
import           Czz.Coverage.Bucket.Bucketing (BucketingName(ZeroOneMany))
import qualified Czz.Log as Log
import qualified Czz.Log.Concurrent as CLog
import qualified Czz.KLimited as KLimit
import qualified Czz.State as State
import qualified Czz.Stop as Stop

import qualified Czz.JVM as Main
import qualified Czz.JVM.Config.Type as Conf

tests :: IO Tasty.TestTree
tests = do
  stop <- Stop.new
  let jvmConf =
        Conf.JVMConfig
        { Conf.classPath = ["test/java"]
        , Conf.jars = []
        , Conf.entryClass = "Main"
        , Conf.entryMethod = "main"
        }
  let fuzzConf =
        CConf.FuzzConfig
        { CConf.bucketing = ZeroOneMany
        , CConf.gas = Nothing
        , CConf.jobs = 1
        , CConf.pathLen = 1
        , CConf.seed = Nothing  -- Just 0
        , CConf.tries = Just 10
        , CConf.stateDir = Nothing
        }

  let assertFinalState cf logger f =
        TastyH.testCase (Conf.entryClass cf) $ do
          Main.fuzz cf fuzzConf stop logger logger >>=
            \case
              Left err -> error (show err)
              Right finalState -> f finalState

  let expectNoBug cf logger prog =
        KLimit.withSomeKLimit 1 $
          assertFinalState (cf { Conf.entryClass = prog }) logger $ \fs ->
              TastyH.assertBool
                ("Expected no bug in " ++ prog)
                (not (State.hasBug fs))

  let expectBug cf logger prog =
        KLimit.withSomeKLimit 1 $
          assertFinalState (cf { Conf.entryClass = prog }) logger $ \fs ->
              TastyH.assertBool
                ("Expected bug in " ++ prog)
                (State.hasBug fs)

  stdStreams <- Lock.new Hand.stdStreams
  return $
    let logger prog =
          if False  -- TODO(lb)
          then
            fmap ((Text.pack prog <> ": ") <>) >$<
              CLog.logStdout Log.Debug stdStreams
          else Log.void
        bug prog = expectBug jvmConf (logger prog) prog
        -- noBug prog = expectNoBug conf (logger prog) prog
    in Tasty.testGroup "Tests"
          [ bug "PrintArg0"
          -- TODO(lb):
          -- , noBug "HelloWorld"
          ]
