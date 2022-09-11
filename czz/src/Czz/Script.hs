{-# LANGUAGE LambdaCase #-}

module Czz.Script
  ( run
  )
where

import           Data.Function ((&))
import qualified Control.Monad.Except as Exc

import qualified Language.Scheme.Core as LSC
import qualified Language.Scheme.Types as LST
import qualified Language.Scheme.Variables as LSV

import           Czz.Config.Type (ScriptConfig)
import qualified Czz.Config.Type as Conf

globalEnv :: [((Char, String), LST.LispVal)]
globalEnv =
  [((LSV.varNamespace, "args"), LST.String "foo")]

run :: ScriptConfig -> IO ()
run conf = do
  r5rsEnv <- LSC.r5rsEnv
  env <- LSV.extendEnv r5rsEnv globalEnv
  let runIOThrows = LSC.runIOThrows . Exc.liftM show
  let loadExpr = LST.List [LST.Atom "load", LST.String (Conf.script conf)]
  (LSC.evalLisp env loadExpr & runIOThrows) >>=
    \case
      Just errMsg -> putStrLn errMsg
      _  -> return ()
