module Czz.Stop
  ( Stop
  , new
  , send
  , should
  )
where

import           Prelude hiding (div, log)
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Maybe as Maybe

newtype Stop = Stop { getStop :: MVar () }

new :: IO Stop
new = Stop <$> MVar.newEmptyMVar

send :: Stop -> IO ()
send = flip MVar.putMVar () . getStop

should :: Stop -> IO Bool
should = fmap Maybe.isJust . MVar.tryReadMVar . getStop
