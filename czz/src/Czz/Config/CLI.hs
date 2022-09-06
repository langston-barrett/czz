module Czz.Config.CLI
  ( parser
  )
where

import qualified Options.Applicative as Opt

import qualified Czz.Log as Log
import           Czz.Config.Type (Config)
import qualified Czz.Config.Type as Conf
import           Czz.Coverage.Bucket.Bucketing (BucketingName(ZeroOneMany))

parser :: Opt.Parser Config
parser =
  Conf.Config
  <$> Opt.option Opt.auto
      ( Opt.long "bucketing"
        <> Opt.short 'b'
        <> Opt.metavar "STRATEGY"
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
  <*> Opt.optional
      ( Opt.option Opt.auto
        ( Opt.long "tries"
          <> Opt.short 't'
          <> Opt.metavar "NUM"
          <> Opt.help "Number of times to try to get new coverage before giving up"
        )
      )
  <*> Opt.option Opt.auto
      ( Opt.long "verbosity"
        <> Opt.short 'v'
        <> Opt.metavar "VERB"
        <> Opt.value Log.Info
        <> Opt.help "Verbosity"
      )
