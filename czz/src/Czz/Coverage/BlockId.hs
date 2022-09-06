{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Czz.Coverage.BlockId
  ( BlockId
  , new
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as AesonTH
import           Data.Hashable (Hashable)
import           GHC.Generics (Generic)

import           What4.FunctionName (FunctionName)

import           Czz.Orphans ()

-- | Identifier for a specific basic block, consisting of function name and BB ID
data BlockId = BlockId !FunctionName !Int
  deriving (Eq, Generic, Ord, Show)

instance Hashable BlockId where

new :: FunctionName -> Int -> BlockId
new = BlockId

$(AesonTH.deriveJSON Aeson.defaultOptions ''BlockId)
