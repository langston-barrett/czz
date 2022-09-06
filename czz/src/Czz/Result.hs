{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Czz.Result
  ( Result(..)
  , isBug
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as AesonTH
import           Data.Text (Text)

import           Czz.Orphans ()
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

$(AesonTH.deriveJSON Aeson.defaultOptions ''Result)
