{-# LANGUAGE LambdaCase #-}

module Test (tests) where

import qualified Control.Monad.Except as Exc
import qualified Data.List as List
import qualified System.Directory as Dir
import qualified System.IO.Silently as Sil

import qualified Language.Scheme.Core as LSC
import qualified Language.Scheme.Types as LST

import qualified Language.Scheme.Data.Maybe as SMaybe
import qualified Language.Scheme.Data.Word as SWord

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as TastyG

tests :: IO Tasty.TestTree
tests = do
  let dir = "test/scm"
  let withDir = ((dir ++ "/") ++)
  env <-
    SMaybe.extendEnv' =<<
      SWord.extendEnv' =<<
        LSC.r5rsEnv

  let checkOutput prog =
        let gold = withDir (replaceSuf ".scm" ".good" prog)
            out = withDir (replaceSuf ".scm" ".out" prog)
        in TastyG.goldenVsFile (prog <> " output") gold out $ do
             let loadExpr =
                   LST.List [LST.Atom "load", LST.String (withDir prog)]
             -- TODO(lb): catch errors
             s <-
              Sil.capture_
                (Exc.runExceptT (LSC.evalLisp env loadExpr) >>=
                  \case
                    Left err -> print err
                    Right _v -> return ())
             writeFile out s

  scms <- filter (".scm" `List.isSuffixOf`) <$> Dir.listDirectory dir
  return (Tasty.testGroup "Tests" (map checkOutput scms))
  where
    replaceSuf suf suf' =
      reverse . (reverse suf' ++) . drop (length suf) . reverse
