{-# LANGUAGE OverloadedStrings #-}

module Czz.Random
  ( genByteString
  , genCString
  , pickSeq
  , pickSet
  , pickVec
  )
where

import           Data.ByteString (ByteString)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified System.Random as Random

genByteString :: (Int, Int) -> IO ByteString
genByteString range = do
  len <- Random.randomRIO range
  gen <- Random.getStdGen
  let (bs, gen') = Random.genByteString len gen
  Random.setStdGen gen'
  return bs

genCString :: (Int, Int) -> IO ByteString
genCString = fmap (<> "\0") . genByteString

-- TODO(lb): off-by-one?
pickSeq :: Seq a -> IO (Maybe a)
pickSeq s = do
  if Seq.null s
  then return Nothing
  else Just . (s `Seq.index`) <$> Random.randomRIO (0, max 0 (Seq.length s - 1))

pickSet :: Set a -> IO (Maybe a)
pickSet = pickVec . Vec.fromList . Set.toList

-- TODO(lb): off-by-one?
pickVec :: Vector a -> IO (Maybe a)
pickVec v =
  if Vec.null v
  then return Nothing
  else Just . (v Vec.!) <$> Random.randomRIO (0, max 0 (Vec.length v - 1))
