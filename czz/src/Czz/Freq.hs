{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Czz.Freq
  ( Freq
  , empty
  , inc
  , incBy
  , map
  , count
  , sorted
  )
where

import           Prelude hiding (map)
import           Data.Hashable (Hashable)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import           Czz.Count (Count)
import qualified Czz.Count as Count

newtype Freq k = Freq { getFreq :: Map k Count }
  deriving (Eq, Hashable, Ord, Show)

empty :: Freq k
empty = Freq Map.empty

inc :: Ord k => k -> Freq k -> Freq k
inc k = incBy k Count.one

incBy :: Ord k => k -> Count -> Freq k -> Freq k
incBy k c = Freq . Map.insertWith Count.plus k c . getFreq

map :: (Count -> Count) -> Freq k -> Freq k
map f = Freq . Map.map f . getFreq

count :: Ord k => [k] -> Freq k
count = foldr inc empty

sorted :: Ord k => Freq k -> Seq (k, Count)
sorted = Seq.sortOn snd . Seq.fromList . Map.toList . getFreq
