{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Czz.Script
  ( run
  )
where

import           Data.Function ((&))
import           Control.Monad (foldM)
import qualified Control.Monad.Except as Exc
import           Control.Monad.IO.Class (liftIO)

import qualified Lang.Crucible.Backend as C
import           Lang.Crucible.Backend (IsSymInterface)

import qualified Language.Scheme.Core as LSC
import qualified Language.Scheme.Types as LST
import qualified Language.Scheme.Variables as LSV

import           Language.Scheme.CustFunc (evalHuskable)
import           Language.Scheme.From ()

import qualified Language.Scheme.Data.Word as LSWord

import qualified Language.Scheme.ByteString as LSBS

import qualified Language.Scheme.What4 as LSWhat4

import qualified Czz.Concurrent.Lock as Lock
import qualified Czz.Concurrent.Handle as Hand
import           Czz.Config.Type (BaseConfig, ScriptConfig)
import qualified Czz.Config.Type as Conf
import qualified Czz.Log.Concurrent as CLog
import qualified Czz.Run as Run
import qualified Czz.Script.API as API

globalEnv :: [((Char, String), LST.LispVal)]
globalEnv =
  [((LSV.varNamespace, "hello"), hello)]
  where
    hello = LST.CustFunc (evalHuskable helloImpl)
    helloImpl :: String -> LST.IOThrowsError LST.LispVal
    helloImpl s = do
      liftIO (putStrLn ("Hello, " ++ s))
      return (LST.List [])

run ::
  BaseConfig ->
  ScriptConfig ->
  (forall sym. IsSymInterface sym => sym -> LST.Env -> IO LST.Env) ->
  IO ()
run baseConf scriptConf extraLibs = do
  let v = Conf.verbosity baseConf
  let cap = 4096 -- TODO(lb): Good default? Configurable?
  lss <- Lock.new Hand.stdStreams
  CLog.withStdoutLogger v lss cap $ \(_tid, stdoutLogger) ->
    CLog.withStderrLogger v lss cap $ \(_tid, stderrLogger) -> do
      Run.withZ3 $ \bak -> do
        let sym = C.backendGetSym bak
        r5rsEnv <- LSC.r5rsEnv
        let libs =
              [ extraLibs sym
              , API.extendEnv stdoutLogger stderrLogger
              , LSWord.extendEnv "word"
              , LSBS.extendEnv "bytes"
              , LSWhat4.extendEnv sym "czz"
              , flip LSV.extendEnv globalEnv
              ]
        env <- foldM (\env lib -> lib env) r5rsEnv libs
        let runIOThrows = LSC.runIOThrows . Exc.liftM show
        let loadExpr =
              LST.List [LST.Atom "load", LST.String (Conf.script scriptConf)]
        (LSC.evalLisp env loadExpr & runIOThrows) >>=
          \case
            Just errMsg -> putStrLn errMsg
            _  -> return ()
