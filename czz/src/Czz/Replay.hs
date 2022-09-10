{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Czz.Replay
  ( replay
  )
where

import           Prelude hiding (log)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Exit as Exit

-- crucible
import           Lang.Crucible.CFG.Extension (IsSyntaxExtension)

import qualified Czz.Concurrent.Lock as Lock
import qualified Czz.Concurrent.Handle as Hand
import qualified Czz.Config.Type as Conf
import           Czz.Fuzz.Type
import qualified Czz.KLimited as KLimited
import qualified Czz.Log as Log
import qualified Czz.Log.Concurrent as CLog
import           Czz.Record (Record)
import qualified Czz.Run as Run
import           Czz.Seed (Seed)
import qualified Czz.Seed as Seed
import           Czz.SysTrace (Time(End))

replay ::
  Aeson.FromJSON env =>
  Aeson.FromJSON eff =>
  IsSyntaxExtension ext =>
  Conf.BaseConfig ->
  Conf.ReplayConfig ->
  Fuzzer ext env eff 1 fb ->
  IO (Record env eff 1 fb)
replay baseConf conf fuzzer = do
  let s = Conf.verbosity baseConf
  let cap = 4096 -- TODO(lb): Good default? Configurable?
  lss <- Lock.new Hand.stdStreams
  CLog.withStdoutLogger s lss cap $ \(_tid, stdoutLogger) ->
    CLog.withStderrLogger s lss cap $ \(_tid, stderrLogger) -> do
      seed <- Log.with stderrLogger seedFromFile
      Log.with stdoutLogger $ do
        -- No need to track coverage, so track with maximal granularity
        KLimited.withKnownKLimit $ do
          Run.runWithZ3 False (Seed.rewind seed) fuzzer
  where
    seedFromFile ::
      Log.Has Text =>
      Aeson.FromJSON env =>
      Aeson.FromJSON eff =>
      IO (Seed 'End env eff)
    seedFromFile = do
      -- TODO(lb): better error message
      bytes <- BSL.readFile (Conf.seedFile conf)
      case Aeson.eitherDecode bytes of
        Left err -> do
          Log.error (Text.pack err)
          Exit.exitFailure
        Right seed -> return seed
