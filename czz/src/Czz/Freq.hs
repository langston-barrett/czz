{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Czz.Freq
  ( Freq
  , empty
  , toList
  , inc
  , singleton
  , incBy
  , map
  , count
  , sorted
  )
where

import           Prelude hiding (map)
import qualified Data.Aeson as Aeson
import           Data.Hashable (Hashable)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)

import           Czz.Count (Count)
import qualified Czz.Count as Count

-- | Count frequencies, i.e., occurrences of @k@s.
--
-- Internal note: A key not being present is semantically the same as it having
-- a value of 'Count.zero'.
newtype Freq k = Freq { getFreq :: Map k Count }
  deriving (Eq, Generic, Hashable, Ord, Show)

empty :: Ord k => Freq k
empty = Freq Map.empty

-- | Convert to list.
--
-- It is undefined whether or not the list will include pairs with 'Count.zero'
-- as their second component; they may be omitted.
--
-- Equations:
--
-- @toList 'empty' == []@
toList :: Ord k => Freq k -> [(k, Count)]
toList = Map.toList . getFreq

-- | Increment the occurrences of a key @k@ by 'Count.one'.
--
-- Equations:
--
-- @forall k. 'inc' k == 'incBy' k 'Count.one'@
--
-- @forall k. 'toList' (inc k 'empty') == [(k, 'Count.one')]@
inc :: Ord k => k -> Freq k -> Freq k
inc k = incBy k Count.one

-- | @forall k. singleton k == 'inc' k 'empty'@
singleton :: Ord k => k -> Freq k
singleton k = inc k empty

-- | Increment the occurrences of a key @k@ by some amount.
incBy :: Ord k => k -> Count -> Freq k -> Freq k
incBy k c = Freq . Map.insertWith Count.plus k c . getFreq

-- | Map over the counts of all of the occurrences.
map :: Ord k => (Count -> Count) -> Freq k -> Freq k
map f = Freq . Map.map f . getFreq

-- | Count the occurrences of each item in the list.
count :: Ord k => [k] -> Freq k
count = foldr inc empty

sorted :: Ord k => Freq k -> Seq (k, Count)
sorted = Seq.sortOn snd . Seq.fromList . Map.toList . getFreq

-- | Merges the counts with 'Count.plus'.
instance Ord k => Semigroup (Freq k) where
  x <> y = Freq (Map.unionWith Count.plus (getFreq x) (getFreq y))

-- | @'mempty' == 'empty'@
instance Ord k => Monoid (Freq k) where
  mempty = empty

--------------------------------------------------------------------------------
-- JSON

instance Aeson.ToJSONKey k => Aeson.ToJSON (Freq k) where
  toJSON = Aeson.toJSON . getFreq
  toEncoding = Aeson.toEncoding . getFreq

instance (Aeson.FromJSONKey k, Ord k) => Aeson.FromJSON (Freq k) where
  parseJSON v = Freq <$> Aeson.parseJSON v
