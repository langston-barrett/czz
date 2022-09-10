module Czz.Config.CLI
  ( fuzzConfig
  , replayConfig
  , config
  )
where

import qualified Data.Foldable as Fold
import qualified Options.Applicative as Opt

import qualified Czz.Log as Log
import           Czz.Config.Type (Config, FuzzConfig, ReplayConfig)
import qualified Czz.Config.Type as Conf
import           Czz.Coverage.Bucket.Bucketing (BucketingName(ZeroOneMany))

fuzzConfig :: Opt.Parser FuzzConfig
fuzzConfig =
  Conf.FuzzConfig
  <$> Opt.option Opt.auto
      ( Opt.long "bucketing"
        <> Opt.short 'b'
        <> Opt.metavar "STRAT"
        <> Opt.value ZeroOneMany
        <> Opt.help "Bucketing strategy, Log2 or ZeroOneMany (default)"
      )
  <*> Opt.optional
      ( Opt.option Opt.auto
        ( Opt.long "gas"
          <> Opt.short 'g'
          <> Opt.metavar "NUM"
          <> Opt.help "Maximum number of executions"
        )
      )
  <*> Opt.option Opt.auto
      ( Opt.long "jobs"
        <> Opt.short 'j'
        <> Opt.metavar "NUM"
        <> Opt.value 1
        <> Opt.help "Number of jobs to run concurrently. Default is 1."
      )
  <*> Opt.option Opt.auto
      ( Opt.long "path-len"
        <> Opt.short 'l'
        <> Opt.metavar "NUM"
        <> Opt.value 2  -- Like AFL
        <> Opt.help "Number of basic blocks to track in each path. Default is 2, i.e., edge coverage. 0 means unlimited."
      )
  <*> Opt.optional
      ( Opt.option Opt.auto
        ( Opt.long "seed"
          <> Opt.short 's'
          <> Opt.metavar "SEED"
          <> Opt.help "Seed, default is randomly generated"
        )
      )
  <*> Opt.option Opt.auto
      ( Opt.long "state"
        <> Opt.metavar "DIR"
        <> Opt.value (Just "czz")
        <> Opt.help "Directory containing fuzzer state"
      )
  <*> Opt.optional
      ( Opt.option Opt.auto
        ( Opt.long "tries"
          <> Opt.short 't'
          <> Opt.metavar "NUM"
          <> Opt.help "Number of times to try to get new coverage before giving up"
        )
      )

replayConfig :: Opt.Parser ReplayConfig
replayConfig =
  Conf.ReplayConfig
  <$> Opt.strArgument
      ( Opt.metavar "SEED"
        <> Opt.help "Seed file (.json)"
      )

config :: Opt.Parser Config
config =
  Conf.Config
  <$> baseParser
  <*> cmdParser
  where
    cmdParser =
      Fold.asum
        [ Conf.CmdFuzz <$>
            Opt.hsubparser
              ( Opt.command
                  "fuzz"
                  ( Opt.info
                      (fuzzConfig Opt.<**> Opt.helper)
                      (Opt.fullDesc <> Opt.progDesc "Fuzz a program")
                  )
              )
        , Conf.CmdReplay <$>
            Opt.hsubparser
              ( Opt.command
                  "replay"
                  ( Opt.info
                      (replayConfig Opt.<**> Opt.helper)
                      (Opt.fullDesc <> Opt.progDesc "Replay a seed")
                  )
              )
        ]

    baseParser =
      Conf.BaseConfig
      <$> Opt.option Opt.auto
          ( Opt.long "verbosity"
            <> Opt.short 'v'
            <> Opt.metavar "VERB"
            <> Opt.value Log.Info
            <> Opt.help "Verbosity"
          )
