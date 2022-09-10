module Czz.LLVM.Config.CLI
  ( llvmConfig
  , cliConfig
  )
where

import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as Opt hiding (fullDesc)

import qualified Czz.Doc as CDoc
import qualified Czz.Config.CLI as Czz.Conf.CLI

import           Czz.LLVM.Config.Type (Config)
import qualified Czz.LLVM.Config.Type as Conf

llvmConfig :: Opt.Parser Conf.LLVMConfig
llvmConfig =
  Conf.LLVMConfig
  <$> Opt.strArgument
      ( Opt.metavar "PROG"
        <> Opt.help "Path to LLVM bitcode module (.bc)"
      )
  <*> Opt.option Opt.auto
      ( Opt.long "entrypoint"
        <> Opt.short 'e'
        <> Opt.metavar "FUNC"
        <> Opt.value "main"
        <> Opt.help "Entry point; must take no arguments, or arguments like main (i32, i8**) or (i32, i8**, i8**). Default is main."
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

cliConfig :: IO Config
cliConfig =
  Opt.customExecParser
    (Opt.prefs Opt.showHelpOnError)
    (Opt.info
      (parser Opt.<**> Opt.helper)
      ( Opt.fullDesc
        <> Opt.progDesc "Whole-program fuzzing for LLVM."
        <> Opt.header "czz-llvm - Whole program fuzzing for LLVM"
      )
    )
    { Opt.infoFooter =
        Opt.vsepChunks
          [ Opt.paragraph ("Read more at " ++ CDoc.docLink)
          ]
    }
  where
    parser :: Opt.Parser Config
    parser =
      Conf.Config
      <$> Czz.Conf.CLI.config
      <*> llvmConfig
