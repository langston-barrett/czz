{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Czz.Coverage.Path
  ( Path
  , empty
  , length
  , snoc
  )
where

import           Prelude hiding (length)

import           Data.Hashable (Hashable)

import           Czz.Coverage.BlockId (BlockId)
import           Czz.KLimited (KLimited, IsKLimited)
import qualified Czz.KLimited as KLimit

newtype Path k = Path { getPath :: KLimited k BlockId }
  deriving (Eq, Hashable, Ord, Show)

empty :: IsKLimited k => Path k
empty = Path KLimit.empty

length :: Path k -> Int
length = KLimit.length . getPath

snoc :: IsKLimited k => Path k -> BlockId -> Path k
snoc p x = Path (KLimit.snoc (getPath p) x)
