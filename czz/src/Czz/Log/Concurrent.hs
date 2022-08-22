{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Czz.Log.Concurrent
  ( withThreadId
  , pfxThreadId
  , forkWithBoundedChan
  , logHandle
  , logStderr
  , logStdout
  , forkStdoutLogger
  , forkStderrLogger
  , forkReadHandle
  )
where

import           Control.Monad (unless)
import           Control.Concurrent (ThreadId)
import qualified Control.Concurrent as Con
import qualified Control.Concurrent.STM.TBQueue as TBQ
import qualified Control.Concurrent.STM as STM
import           Control.Exception.Base (SomeException)
import           Data.Functor.Contravariant ((>$<))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import           Numeric.Natural (Natural)
import           System.IO (Handle)
import qualified System.IO as IO

import           Czz.Concurrent.Lock (Locked)
import           Czz.Concurrent.Handle (StdStreams)
import qualified Czz.Concurrent.Handle as Hand
import           Czz.Log (Logger, Severity, Msg)
import qualified Czz.Log as Log

withThreadId ::
  Logger msg ->
  (ThreadId -> msg -> msg) ->
  IO (Logger msg)
withThreadId logger adapt = do
  tid <- Con.myThreadId
  return (adapt tid >$< logger)

pfxThreadId ::
  Functor f =>
  Logger (f Text) ->
  IO (Logger (f Text))
pfxThreadId logger =
  withThreadId
    logger
    (\tid msg ->
       let tidTxt = Text.pack (drop (length ("ThreadId " :: String)) (show tid))
           pfx t = "[" <> lpad 7 '0' tidTxt <> "] " <> t
       in pfx <$> msg)
  where
    lpad :: Int -> Char -> Text -> Text
    lpad num c t =
      if Text.length t < num
      then lpad (num - 1) c (Text.cons c t)
      else t

forkWithBoundedChan ::
  (SomeException -> IO ()) ->
  Natural ->
  Logger msg ->
  IO (ThreadId, Logger msg)
forkWithBoundedChan onExc bound logger = do
  q <- TBQ.newTBQueueIO bound
  let onExit =
        \case
          Left err -> onExc err
          Right () -> pure ()
  -- TODO(lb): is forkFinally enough to handle all exceptions?
  let loop = do
        Log.log logger =<< STM.atomically (TBQ.readTBQueue q)
        loop
  tid <- Con.forkFinally loop onExit
  return (tid, Log.bare (STM.atomically . TBQ.writeTBQueue q))

logHandle :: Severity -> Locked Handle -> Logger (Msg Text)
logHandle s lock = Log.new s (Hand.lhPutStrLn lock)

logStdout :: Severity -> Locked StdStreams -> Logger (Msg Text)
logStdout s lss = Log.new s (Hand.lPutStrLn lss)

logStderr :: Severity -> Locked StdStreams -> Logger (Msg Text)
logStderr s lss = Log.new s (Hand.lPutErrLn lss)

forkStdoutLogger ::
  Severity ->
  Locked StdStreams ->
  Natural ->
  IO (ThreadId, Logger (Msg Text))
forkStdoutLogger s lss cap = do
  let msg = "[ERROR] stdout logging thread exited! "
  let onError = Hand.lPutErrLn lss . Text.pack . (msg ++) . show
  forkWithBoundedChan onError cap (logStdout s lss)

forkStderrLogger ::
  Severity ->
  Locked StdStreams ->
  Natural ->
  IO (ThreadId, Logger (Msg Text))
forkStderrLogger s lss cap = do
  let msg = "[ERROR] stderr logging thread exited! "
  let onError = Hand.lPutErrLn lss . Text.pack . (msg ++) . show
  forkWithBoundedChan onError cap (logStderr s lss)

-- | Fork a new thread that reads each line from a handle and forwards it to the
-- given logger until the handle is closed.
forkReadHandle ::
  (SomeException -> IO ()) ->
  Handle ->
  Logger Text ->
  IO ThreadId
forkReadHandle onExc h logger = do
  let onExit =
        \case
          Left err -> onExc err
          Right () -> pure ()
  let loop = do
        isClosed <- IO.hIsEOF h
        unless isClosed $ do
          isEOF <- IO.hIsEOF h
          if isEOF
          then Con.threadDelay 1000 >> loop
          else do
            Log.log logger =<< TextIO.hGetLine h
            loop
  Con.forkFinally loop onExit
