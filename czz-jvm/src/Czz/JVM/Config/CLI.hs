module Czz.JVM.Config.CLI
  ( jvmConfig
  , config
  )
where

import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as Opt hiding (fullDesc)

import qualified Czz.Doc as CDoc
import qualified Czz.Config.CLI as Czz.Conf.CLI

import           Czz.JVM.Config.Type (Config, JVMConfig)
import qualified Czz.JVM.Config.Type as Conf

jvmConfig :: Opt.Parser JVMConfig
jvmConfig =
  Conf.JVMConfig
  <$> Opt.many
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
  <*> Opt.strOption
      ( Opt.long "entry-class"
        <> Opt.short 'c'
        <> Opt.metavar "CLASS"
        <> Opt.value "Main"
        <> Opt.help "Class with entry point method (default: Main)"
      )
  <*> Opt.strOption
      ( Opt.long "entry-method"
        <> Opt.short 'e'
        <> Opt.metavar "METHOD"
        <> Opt.value "main"
        <> Opt.help "Entry point; must take String[] argument (default: main)"
      )

config :: IO Config
config =
  Opt.customExecParser
    (Opt.prefs Opt.showHelpOnError)
    (Opt.info
      (parser Opt.<**> Opt.helper)
      ( Opt.fullDesc
        <> Opt.progDesc "Whole-program fuzzing for JVM."
        <> Opt.header "czz-jvm - Whole program fuzzing for JVM"
      )
    )
    { Opt.infoFooter =
        Opt.vsepChunks
          [ Opt.paragraph ("Read more at " ++ CDoc.docLink)
          ]
    }
  where
    -- TODO(lb): defaults?
    parser :: Opt.Parser Config
    parser =
      Conf.Config
      <$> Czz.Conf.CLI.config
      <*> jvmConfig
