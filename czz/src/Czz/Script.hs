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

import qualified Language.Scheme.Core as LSC
import qualified Language.Scheme.Types as LST
import qualified Language.Scheme.Variables as LSV

import           Language.Scheme.CustFunc (evalHuskable)
import           Language.Scheme.From ()

import qualified Language.Scheme.Data.Maybe as SMaybe
import qualified Language.Scheme.Data.Word as SWord

import qualified Language.Scheme.ByteString as SBS

import qualified Language.Scheme.What4 as SWhat4

import qualified Czz.Concurrent.Lock as Lock
import qualified Czz.Concurrent.Handle as Hand
import           Czz.Config.Type (BaseConfig, ScriptConfig)
import qualified Czz.Config.Type as Conf
import qualified Czz.Log.Concurrent as CLog
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
  (LST.Env -> IO LST.Env) ->
  IO ()
run baseConf scriptConf extraLibs = do
  let v = Conf.verbosity baseConf
  let cap = 4096 -- TODO(lb): Good default? Configurable?
  lss <- Lock.new Hand.stdStreams
  CLog.withStdoutLogger v lss cap $ \(_tid, stdoutLogger) ->
    CLog.withStderrLogger v lss cap $ \(_tid, stderrLogger) -> do
      r5rsEnv <- LSC.r5rsEnv
      let libs =
            [ extraLibs
            , API.extendEnv stdoutLogger stderrLogger
            , SWord.extendEnv "word"
            , SMaybe.extendEnv "maybe"
            , SBS.extendEnv "bytes"
            , SWhat4.extendEnv "czz"
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
