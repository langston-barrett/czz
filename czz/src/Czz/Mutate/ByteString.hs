module Czz.Mutate.ByteString
  ( any
  , mut
  , new
  , replaceExact
  , replaceSmaller
  , dropFront
  , dropBack
  )
where

import           Prelude hiding (any)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Maybe as Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified System.Random as Random

import qualified Czz.Random as Rand

muts :: Seq (ByteString -> IO ByteString)
muts =
  Seq.fromList
    [ replaceSmaller
    , replaceExact
    , dropFront
    , dropBack
    ]

any :: (Int, Int) -> ByteString -> IO ByteString
any r bs = do
  let f _bs = new r
  g <- Rand.pickSeq (f Seq.<| muts)
  Maybe.fromJust g bs

mut :: ByteString -> IO ByteString
mut bs = do
  g <- Rand.pickSeq muts
  Maybe.fromJust g bs

new :: (Int, Int) -> IO ByteString
new = Rand.genByteString

replaceSmaller :: ByteString -> IO ByteString
replaceSmaller bs = Rand.genByteString (0, BS.length bs)

replaceExact :: ByteString -> IO ByteString
replaceExact bs = Rand.genByteString (len, len)
  where len = BS.length bs

dropFront :: ByteString -> IO ByteString
dropFront bs = do
  newSize <- Random.randomRIO (0, BS.length bs)
  return (BS.drop newSize bs)

dropBack :: ByteString -> IO ByteString
dropBack bs = do
  newSize <- Random.randomRIO (0, BS.length bs)
  return (BS.take newSize bs)
