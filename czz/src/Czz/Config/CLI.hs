module Czz.Config.CLI
  ( parser
  )
where

import qualified Options.Applicative as Opt

import           Czz.Config.Type (Config)
import qualified Czz.Config.Type as Conf

parser :: Opt.Parser Config
parser =
  Conf.Config
  <$> Opt.optional
      ( Opt.option Opt.auto
        ( Opt.long "jobs"
          <> Opt.short 'j'
          <> Opt.metavar "NUM"
          <> Opt.help "Number of jobs to run concurrently"
        )
      )
  <*> Opt.optional
      ( Opt.option Opt.auto
        ( Opt.long "path-len"
          <> Opt.short 'l'
          <> Opt.metavar "NUM"
          <> Opt.help "Number of basic blocks to track in each path. Default is 2, i.e., edge coverage. 0 means unlimited."
        )
      )
  <*> Opt.optional
      ( Opt.option Opt.auto
        ( Opt.long "seed"
          <> Opt.short 's'
          <> Opt.metavar "SEED"
          <> Opt.help "Seed"
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
