{-# LANGUAGE DeriveGeneric #-}

module Czz.Coverage.BlockId
  ( BlockId
  , new
  )
where

import           Data.Hashable (Hashable)
import           GHC.Generics (Generic)

import           What4.FunctionName (FunctionName)

-- TODO(lb): doc
data BlockId = BlockId !FunctionName !Int
  deriving (Eq, Generic, Ord, Show)

instance Hashable BlockId where

new :: FunctionName -> Int -> BlockId
new = BlockId
