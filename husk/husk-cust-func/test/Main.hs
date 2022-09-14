module Main (main) where

import qualified Test.Tasty as Tasty

import qualified Test

main :: IO ()
main =
  Tasty.defaultMainWithIngredients ingredients =<< Test.tests
  where
    ingredients = Tasty.defaultIngredients ++ huskCustFuncIngredients
    huskCustFuncIngredients = []
