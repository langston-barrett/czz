{-# LANGUAGE ScopedTypeVariables #-}

module Test (tests) where

import qualified Control.Monad.Except as Exc
import           Data.Bifunctor (first)
import qualified Text.Parsec as Parsec

import qualified Language.Scheme.Core as LSC
import qualified Language.Scheme.Parser as LSP
import qualified Language.Scheme.Types as LST

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as TastyH

import qualified Language.Scheme.From as From

tests :: IO Tasty.TestTree
tests =
  return $ Tasty.testGroup "Tests" [fromTests]

fromTests :: Tasty.TestTree
fromTests =
  Tasty.testGroup
    "From"
    [ success "0" (0 :: Integer)
    , success "\"\"" ""
    , TastyH.testCase "From (lambda (x) x)" $ do
        (f :: Integer -> LST.IOThrowsError Integer) <-
          fromIO' "(lambda (x) x)"
        app <- runIOThrows (f 0)
        0 TastyH.@=? app
    ]
  where

    runIOThrows :: LST.IOThrowsError a -> IO a
    runIOThrows c = do
      v <- Exc.runExceptT c
      case v of
        Left err -> error (show err)
        Right a -> return a

    success :: Eq a => Show a => From.From a => String -> a -> Tasty.TestTree
    success s expect =
      TastyH.testCase ("From " ++ s) $ do
        actual <- from s
        Right expect TastyH.@=? actual

    from :: From.From a => String -> IO (Either String a)
    from s = first show . From.from <$> p s

    -- fromIO :: From.FromIO a => String -> IO (Either String a)
    -- fromIO = fmap (first show) . From.fromIO . p

    fromIO' :: From.FromIO a => String -> IO a
    fromIO' s = do
      val <- From.fromIO =<< p s
      case val of
        Left err -> error (show err)
        Right v -> return v

    p :: String -> IO LST.LispVal
    p s = do
      env <- LSC.r5rsEnv
      case Parsec.parse LSP.mainParser "test" s of
        Left err -> error (show err)
        Right v -> runIOThrows (LSC.evalLisp env v)
