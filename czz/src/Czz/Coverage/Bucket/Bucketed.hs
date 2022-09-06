{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Czz.Coverage.Bucket.Bucketed
  ( BucketedCoverage
  , empty
  , bucket
  , merge
  )
where

import           Prelude hiding (lookup, log)
import           Data.Bifunctor (second)
import           Data.Hashable (Hashable)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)

import           Czz.Count (Count)
import           Czz.Coverage.Bucket.Bucketing (Bucketing)
import qualified Czz.Coverage.Bucket.Bucketing as Bucketing
import           Czz.Coverage.Path (Path)
import           Czz.Coverage.Seed (SeedCoverage)
import qualified Czz.Coverage.Seed as CSeed
import qualified Czz.Freq as Freq

-- | Total coverage achieved by all executions/seeds.
newtype BucketedCoverage k
  = BucketedCoverage { getBucketedCoverage :: Map (Path k) (Set Count) }
  deriving (Eq, Generic, Hashable, Ord, Semigroup, Show)

empty :: BucketedCoverage k
empty = BucketedCoverage Map.empty

bucket :: Bucketing -> SeedCoverage k -> BucketedCoverage k
bucket b =
  BucketedCoverage .
    Map.fromList .
    map (second Set.singleton) .
    Freq.toList .
    Freq.map (Bucketing.bucket b) .
    CSeed.freq

insert :: Path k -> Set Count -> BucketedCoverage k -> BucketedCoverage k
insert k v =
  BucketedCoverage . Map.insertWith Set.union k v . getBucketedCoverage

lookup :: Path k -> BucketedCoverage k -> Maybe (Set Count)
lookup k = Map.lookup k . getBucketedCoverage

merge :: BucketedCoverage k -> BucketedCoverage k -> (BucketedCoverage k, Bool)
merge bc1 bc2 =
  foldr
    (\(path, counts) (result, anyNew) -> go path counts result anyNew)
    (bc2, False)
    (Map.toList (getBucketedCoverage bc1))
  where
    go ::
      Path k ->
      Set Count ->
      BucketedCoverage k ->
      Bool ->
      (BucketedCoverage k, Bool)
    go path counts result anyNew =
      let anyNew' =
            if anyNew
            then True
            else maybe True (not . (counts `Set.isSubsetOf`)) (lookup path result)
      in (insert path counts result, anyNew')
