module Main (main) where

import           System.Exit (exitWith)

import qualified Czz.LLVM.Main as Czz (main)

main :: IO ()
main = do
  exitWith =<< Czz.main
