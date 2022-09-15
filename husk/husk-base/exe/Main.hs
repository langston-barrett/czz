{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Data.Function ((&))
import           Control.Monad (foldM)
import qualified Control.Monad.Except as Exc
import           Control.Monad.IO.Class (liftIO)
import qualified System.Environment as Env

import qualified Language.Scheme.Core as LSC
import qualified Language.Scheme.Types as LST
import qualified Language.Scheme.Variables as LSV

import           Language.Scheme.Interop.To.Func (toSchemeFunc)

import qualified Language.Scheme.Data.Maybe as SMaybe
import qualified Language.Scheme.Data.Word as SWord

globalEnv :: [((Char, String), LST.LispVal)]
globalEnv = [((LSV.varNamespace, "hello"), hello)]
  where
    hello = LST.CustFunc (toSchemeFunc helloImpl)
    helloImpl :: String -> LST.IOThrowsError LST.LispVal
    helloImpl s = do
      liftIO (putStrLn ("Hello, " ++ s))
      return (LST.List [])

main :: IO ()
main = do
  [scm] <- Env.getArgs
  r5rsEnv <- LSC.r5rsEnv
  let libs =
        [ SWord.extendEnv "word"
        , SMaybe.extendEnv "maybe"
        , flip LSV.extendEnv globalEnv
        ]
  env <- foldM (\env lib -> lib env) r5rsEnv libs
  let runIOThrows = LSC.runIOThrows . Exc.liftM show
  let loadExpr = LST.List [LST.Atom "load", LST.String scm]
  (LSC.evalLisp env loadExpr & runIOThrows) >>=
    \case
      Just errMsg -> putStrLn errMsg
      _  -> return ()
