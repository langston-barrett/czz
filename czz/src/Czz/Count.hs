{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Czz.Count
  ( Count
  , fromWord
  , toWord
  , zero
  , one
  , inc
  , plus
  , absDiff
  , div
  , divWord
  , log
  )
where

import           Prelude hiding (div, log)
import qualified Prelude as Pre
import           Data.Data (Data)
import           Data.Hashable (Hashable)
import           GHC.Generics (Generic)

-- | Type of word-size unsigned integers with limited arithmetic operations.
--
-- Comparison to other types:
--
-- * 'Word' has different semantic connotations - the name implies it is just a
--   bitvector of machine-word size. Furthermore, it exposes operations like @-@
--   which are easily misused (leading to overflow and underflow).
-- * 'Numeric.Natural.Natural' is unbounded, and so doesn't support
--   constant-time operations.
-- * 'Int' is signed.
--
-- Also, this type is unlifted.
newtype Count = Count { getCount :: Word }
  deriving (Bounded, Data, Eq, Enum, Hashable, Generic, Ord, Read, Show)

fromWord :: Count -> Word
fromWord = getCount

toWord :: Count -> Word
toWord = getCount

zero :: Count
zero = Count 0

one :: Count
one = Count 1

inc :: Count -> Count
inc = Count . (+1) . getCount

plus :: Count -> Count -> Count
plus c d = Count (getCount c + getCount d)

-- | Absolute value of the difference between two 'Count'
absDiff :: Count -> Count -> Word
absDiff (Count c1) (Count c2) = max c1 c2 - min c1 c2

-- | Integer (truncating) division
div :: Count -> Count -> Count
div c1 c2 = Count (getCount c1 `Pre.div` getCount c2)

-- | Integer (truncating) division
divWord :: Count -> Word -> Count
divWord c w = c `div` Count w

log :: Word -> Count -> Count
log base c = Count (logn base (getCount c))
  where
    logn b x =
      if x == 1 || x == 0
      then 0
      else 1 + logn b (x `Pre.div` b)
