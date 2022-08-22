module Czz.Concurrent.Lock
  ( Locked
  , new
  , hold
  )
where

import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar

data Locked a = Locked a (MVar ())

new :: a -> IO (Locked a)
new a = Locked a <$> MVar.newEmptyMVar

hold :: Locked a -> (a -> IO b) -> IO b
hold (Locked a lock) f = do
  MVar.putMVar lock ()
  b <- f a
  MVar.takeMVar lock
  return b
