module Czz.LLVM.TUI.Config.CLI
  ( config
  )
where

import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as Opt hiding (fullDesc)

import qualified Czz.Doc as CDoc
import qualified Czz.Config.CLI

import qualified Czz.LLVM.Config.CLI

import           Czz.LLVM.TUI.Config.Type (Config)
import qualified Czz.LLVM.TUI.Config.Type as Conf

config :: IO Config
config =
  Opt.customExecParser
    (Opt.prefs Opt.showHelpOnError)
    (Opt.info
      (parser Opt.<**> Opt.helper)
      ( Opt.fullDesc
        <> Opt.progDesc "Whole-program fuzzing for LLVM."
        <> Opt.header "czz-llvm-tui - Whole program fuzzing for LLVM"
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
      <$> Czz.LLVM.Config.CLI.llvmConfig
      <*> Czz.Config.CLI.fuzzConfig
