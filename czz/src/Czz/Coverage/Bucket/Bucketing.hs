{-# LANGUAGE LambdaCase #-}

module Czz.Coverage.Bucket.Bucketing
  ( Bucketing
  , bucketing
  , bucket
  , log
  , log2
  , zeroOneMany
  , BucketingName(..)
  , fromName
  )
where

import           Prelude hiding (log)

import           Czz.Count (Count)
import qualified Czz.Count as Count

newtype Bucketing
  = Bucketing { getBucketing :: Count -> Count }

bucketing :: (Count -> Count) -> Bucketing
bucketing = Bucketing

bucket :: Bucketing -> Count -> Count
bucket = getBucketing

log :: Word -> Bucketing
log base = Bucketing (Count.log base)

log2 :: Bucketing
log2 = log 2

zeroOneMany :: Bucketing
zeroOneMany = Bucketing $ \c ->
  case Count.toWord c of
    0 -> Count.zero
    1 -> Count.one
    _ -> Count.inc Count.one

--------------------------------------------------------------------------------
-- BucketingName

data BucketingName
  = Log2
  | ZeroOneMany
  deriving (Eq, Ord, Read, Show)

fromName :: BucketingName -> Bucketing
fromName =
  \case
    Log2 -> log2
    ZeroOneMany -> zeroOneMany
