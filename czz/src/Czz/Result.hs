{-# LANGUAGE LambdaCase #-}

module Czz.Result
  ( Result(..)
  , isBug
  )
where

import           Data.Text (Text)

import qualified What4.ProgramLoc as What4

data Result
  = MissingOverride !Text
  | Ok
  | Bug !What4.ProgramLoc !Text -- ^ @ppSimError@
  deriving (Eq, Ord)

isBug :: Result -> Bool
isBug =
  \case
    Bug {} -> True
    _ -> False
