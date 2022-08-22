module Main (main) where

import qualified Test.Tasty as Tasty

import           Option (options)
import qualified Test

main :: IO ()
main =
  Tasty.defaultMainWithIngredients ingredients =<< Test.tests
  where
    ingredients = Tasty.defaultIngredients ++ czzIngredients
    czzIngredients =
      [ Tasty.includingOptions options
      ]
