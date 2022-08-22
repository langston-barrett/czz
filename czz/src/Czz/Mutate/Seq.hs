module Czz.Mutate.Seq
  ( any
  , mut
  , idx
  , new
  , add
  , delete
  )
where

import           Prelude hiding (any)
import qualified Data.Maybe as Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified System.Random as Random

import qualified Czz.Random as Rand

muts ::
  (a -> IO a) ->
  IO a ->
  Seq (Seq a -> IO (Seq a))
muts m a =
  Seq.fromList
    [ add <==< a
    , delete
    , mutElem m
    ]
  where
    f <==< x = \s -> do
      v <- x
      f v s

any ::
  (a -> IO a) ->
  IO a ->
  (Int, Int) ->
  Seq a ->
  IO (Seq a)
any m a r bs = do
  let f _bs = new a r
  g <- Rand.pickSeq (f Seq.<| muts m a)
  Maybe.fromJust g bs

mut ::
  (a -> IO a) ->
  IO a ->
  Seq a ->
  IO (Seq a)
mut m a s = do
  g <- Rand.pickSeq (muts m a)
  Maybe.fromJust g s

new :: IO a -> (Int, Int) -> IO (Seq a)
new a r = do
  len <- Random.randomRIO r
  if len <= 0
    then return Seq.empty
    else Seq.fromList <$> mapM (const a) [1..len - 1]

idx :: Seq a -> IO (Maybe Int)
idx s =
  if Seq.null s
  then return Nothing
  else Just <$> Random.randomRIO (0, Seq.length s - 1)

mutAt :: (a -> IO a) -> Int -> Seq a -> IO (Seq a)
mutAt f i s = do
  v <- f (s `Seq.index` i)
  return (Seq.adjust (const v) i s)

mutElem :: (a -> IO a) -> Seq a -> IO (Seq a)
mutElem f s = do
  mi <- idx s
  case mi of
    Nothing -> return s
    Just i -> mutAt f i s

add :: a -> Seq a -> IO (Seq a)
add item s = do
  mi <- idx s
  case mi of
    Nothing -> return (Seq.singleton item)
    Just i -> return (Seq.insertAt i item s)

delete :: Seq a -> IO (Seq a)
delete s = do
  mi <- idx s
  case mi of
    Nothing -> return s
    Just i -> return (Seq.deleteAt i s)

