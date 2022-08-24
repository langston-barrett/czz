module Main (main) where

import           System.Exit (exitWith)

import qualified Czz.LLVM.TUI as Czz (main)

main :: IO ()
main = exitWith =<< Czz.main
