{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test (tests) where

import qualified Control.Monad.Except as Exc
import           Data.Coerce (coerce)
import           Data.Maybe (isJust)
import qualified Data.Dynamic as Dyn
import qualified Text.Parsec as Parsec

import qualified Language.Scheme.Core as LSC
import qualified Language.Scheme.Parser as LSP
import qualified Language.Scheme.Types as LST

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as TastyH

import qualified Language.Scheme.Interop.From as From
import           Language.Scheme.Interop.Opaque (Opaque(..))
import           Language.Scheme.Interop.To.Func (Ret(..))  -- for coerce
import qualified Language.Scheme.Interop.To.Func as To

tests :: IO Tasty.TestTree
tests =
  return $ Tasty.testGroup "Tests" [fromTests, toTests]

runIOThrows :: LST.IOThrowsError a -> IO a
runIOThrows c = do
  v <- Exc.runExceptT c
  case v of
    Left err -> error (show err)
    Right a -> return a

fromTests :: Tasty.TestTree
fromTests =
  Tasty.testGroup
    "From"
    [ success "0" (0 :: Integer)
    , success "\"\"" ""
    , success "(list 0)" [0 :: Integer]
    , success "(list 0 1)" [0, 1 :: Integer]
    , success "((lambda (x) (list 0)) 0)" [0 :: Integer]
    , TastyH.testCase "(lambda (x) x)" $ do
        f <- from "(lambda (x) x)"
        app <- runIOThrows (f (0 :: Integer))
        (0 :: Integer) TastyH.@=? app
    , TastyH.testCase "(let ((z 5)) (lambda (x) z))" $ do
        f <- from "(let ((z 5)) (lambda (x) z))"
        app <- runIOThrows (f False)
        (5 :: Integer) TastyH.@=? app
    , TastyH.testCase "(let ((f (lambda (x) x))) (lambda (x) (f x)))" $ do
        f <- from "(let ((f (lambda (x) x))) (lambda (x) (f x)))"
        app <- runIOThrows (f False)
        False TastyH.@=? app
    , TastyH.testCase "(lambda (x y) x)" $ do
        f <- from "(lambda (x y) x)"
        app <- runIOThrows (f True False)
        True TastyH.@=? app
    , TastyH.testCase "(lambda (x y) (list x y))" $ do
        f <- from "(lambda (x y) (list x y))"
        app <- runIOThrows (f True False)
        [True, False] TastyH.@=? app
    , TastyH.testCase "(lambda (x y z) (or x y z))" $ do
        f <- from "(lambda (x y z) (or x y z))"
        app <- runIOThrows (f True False False)
        True TastyH.@=? app
    ]
  where

    success :: Eq a => Show a => From.FromScheme a => String -> a -> Tasty.TestTree
    success s expect =
      TastyH.testCase ("From " ++ s) $ do
        actual <- from s
        expect TastyH.@=? actual

    from :: From.FromScheme a => String -> IO a
    from s = do
      val <- Exc.runExceptT . From.fromScheme =<< p s
      case val of
        Left err -> error (show err)
        Right v -> return v

    -- TODO(lb): evalString!
    p :: String -> IO LST.LispVal
    p s = do
      env <- LSC.r5rsEnv
      case Parsec.parse LSP.mainParser "test" s of
        Left err -> error (show err)
        Right v -> runIOThrows (LSC.evalLisp env v)

toTests :: Tasty.TestTree
toTests =
  Tasty.testGroup
    "To"
    [ let id' = coerce (id @Bool) :: Bool -> Ret Bool
          t = LST.Bool True
      in eq "id" id' [t] t
    , let head' = coerce (head @Bool) :: [Bool] -> Ret Bool
          t = LST.Bool True
          f = LST.Bool False
      in eq "head" head' [LST.List [t, f]] t
    , let isJust' = coerce (isJust @Bool) :: Opaque (Maybe Bool) -> Ret Bool
      in eq "isJust" isJust' [LST.Opaque (Dyn.toDyn (Just True))] (LST.Bool True)
    ]
  where
    eq ::
      To.ToSchemeFunc a =>
      String ->
      a ->
      [LST.LispVal] ->
      LST.LispVal ->
      Tasty.TestTree
    eq nm f args expect =
      TastyH.testCase nm $ do
        app <- runIOThrows (To.toSchemeFunc f args)
        expect TastyH.@=? app
