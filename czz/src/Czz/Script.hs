{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Czz.Script
  ( run
  )
where

import           Data.Function ((&))
import qualified Control.Monad.Except as Exc
import           Control.Monad.IO.Class (liftIO)

import qualified Language.Scheme.Core as LSC
import qualified Language.Scheme.Types as LST
import qualified Language.Scheme.Variables as LSV

import           Language.Scheme.CustFunc (evalHuskable)
import           Language.Scheme.From ()

import qualified Language.Scheme.Data.Word as LSWord

import qualified Language.Scheme.ByteString as LSBS

import           Czz.Config.Type (ScriptConfig)
import qualified Czz.Config.Type as Conf

globalEnv :: [((Char, String), LST.LispVal)]
globalEnv =
  [((LSV.varNamespace, "hello"), hello)]
  where
    hello = LST.CustFunc (evalHuskable helloImpl)
    helloImpl :: String -> LST.IOThrowsError LST.LispVal
    helloImpl s = do
      liftIO (putStrLn ("Hello, " ++ s))
      return (LST.List [])

run :: ScriptConfig -> IO ()
run conf = do
  r5rsEnv <- LSC.r5rsEnv
  env <-
    LSWord.extendEnv "word" =<<
      LSBS.extendEnv "bytes" =<<
        LSV.extendEnv r5rsEnv globalEnv
  let runIOThrows = LSC.runIOThrows . Exc.liftM show
  let loadExpr = LST.List [LST.Atom "load", LST.String (Conf.script conf)]
  (LSC.evalLisp env loadExpr & runIOThrows) >>=
    \case
      Just errMsg -> putStrLn errMsg
      _  -> return ()
