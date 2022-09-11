module Czz.LLVM.TUI.Config.Type
  ( Config(..)
  )
where

import           Czz.Config.Type (FuzzConfig)

import           Czz.LLVM.Config.Type (LLVMConfig)

data Config
  = Config
    { llvm :: LLVMConfig
    , fuzz :: FuzzConfig
    }
