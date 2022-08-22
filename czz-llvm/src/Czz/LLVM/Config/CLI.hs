module Czz.LLVM.Config.CLI
  ( cliConfig
  )
where

import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as Opt hiding (fullDesc)

import qualified Czz.Config.CLI as Czz.Conf.CLI

import           Czz.LLVM.Config.Type (Config)
import qualified Czz.LLVM.Config.Type as Conf

cliConfig :: IO Config
cliConfig =
  Opt.customExecParser
    (Opt.prefs Opt.showHelpOnError)
    (Opt.info
      (parser Opt.<**> Opt.helper)
      ( Opt.fullDesc
        <> Opt.progDesc "Fuzz TODO(lb)"
        <> Opt.header "czz - Tool for blah blah blah"
      )
    )
    { Opt.infoFooter =
        Opt.vsepChunks
          [ Opt.paragraph "More detailed info goes here."
          ]
    }
  where
    parser :: Opt.Parser Config
    parser =
      Conf.Config
      <$> Czz.Conf.CLI.parser
      <*> Opt.strArgument
          ( Opt.metavar "PROG"
            <> Opt.help "Path to LLVM bitcode module (.bc)"
          )
      <*> Opt.optional
          ( Opt.option Opt.auto
            ( Opt.long "entrypoint"
              <> Opt.short 'e'
              <> Opt.metavar "FUNC"
              <> Opt.help "Entry point; must take no arguments, or arguments like main (i32, i8**)"
            )
          )
      <*> Opt.many
          ( Opt.strOption
            ( Opt.long "skip"
              <> Opt.metavar "FUNC"
              <> Opt.help "Functions to skip; must be void"
            )
          )
      <*> Opt.flag
          True
          False
          ( Opt.long "not-only-needed"
            <> Opt.help "Don't pass --only-needed to llvm-link"
          )
