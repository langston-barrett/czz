module Czz.Concurrent.Handle
  ( lhPutStrLn
  , StdStreams
  , stdStreams
  , lPutStrLn
  , lPutErrLn
  )
where

import           Data.Text (Text)
import qualified Data.Text.IO as TextIO
import           System.IO (Handle)
import qualified System.IO as IO

import           Czz.Concurrent.Lock (Locked)
import qualified Czz.Concurrent.Lock as Lock

lhPutStrLn :: Locked Handle -> Text -> IO ()
lhPutStrLn lh txt = Lock.hold lh (\h -> TextIO.hPutStrLn h txt)

-- | These streams need to share a lock because otherwise it looks confusing in
-- a terminal.
data StdStreams =
  StdStreams
    { _outLock :: Handle
    , _errLock :: Handle
    }

stdStreams :: StdStreams
stdStreams = StdStreams IO.stdout IO.stderr

lPutStrLn :: Locked StdStreams -> Text -> IO ()
lPutStrLn lss txt =
  Lock.hold lss (\(StdStreams out _err) -> TextIO.hPutStrLn out txt)

lPutErrLn :: Locked StdStreams -> Text -> IO ()
lPutErrLn lss txt =
  Lock.hold lss (\(StdStreams _out err) -> TextIO.hPutStrLn err txt)
