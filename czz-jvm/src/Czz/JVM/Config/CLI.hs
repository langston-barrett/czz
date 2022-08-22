module Czz.JVM.Config.CLI
  ( cliConfig
  )
where

import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as Opt hiding (fullDesc)

import qualified Czz.Config.CLI as Czz.Conf.CLI

import           Czz.JVM.Config.Type (Config)
import qualified Czz.JVM.Config.Type as Conf

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
    -- TODO(lb): defaults?
    parser :: Opt.Parser Config
    parser =
      Conf.Config
      <$> Czz.Conf.CLI.parser
      <*> Opt.many
          ( Opt.strOption
            ( Opt.long "class-path"
              <> Opt.metavar "DIR"
              <> Opt.help "Class path"
            )
          )
      <*> Opt.many
          ( Opt.strOption
            ( Opt.long "jar"
              <> Opt.metavar "JAR"
              <> Opt.help "JAR files"
            )
          )
      <*> Opt.optional
          ( Opt.option Opt.auto
            ( Opt.long "entry-class"
              <> Opt.short 'c'
              <> Opt.metavar "CLASS"
              <> Opt.help "Class with entry point method (default: Main)"
            )
          )
      <*> Opt.optional
          ( Opt.option Opt.auto
            ( Opt.long "entry-method"
              <> Opt.short 'e'
              <> Opt.metavar "METHOD"
              <> Opt.help "Entry point; must take String[] argument (default: main)"
            )
          )
