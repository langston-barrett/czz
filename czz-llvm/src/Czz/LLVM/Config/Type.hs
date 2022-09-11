{-# LANGUAGE StrictData #-}

module Czz.LLVM.Config.Type
  ( LLVMConfig(..)
  , Config(..)
  )
where

import qualified Czz.Config.Type as Czz.Conf.Type

data LLVMConfig
  = LLVMConfig
    { prog :: FilePath
    , entryPoint :: String
    , skip :: [String]
    , onlyNeeded :: Bool
    }

data Config
  = Config
    { common :: Czz.Conf.Type.Config
    , llvm :: LLVMConfig
    }
