{-# LANGUAGE StrictData #-}

module Czz.JVM.Config.Type
  ( JVMConfig(..)
  , Config(..)
  )
where

import qualified Czz.Config.Type as Czz.Conf.Type

data JVMConfig
  = JVMConfig
    { classPath :: [FilePath]
    , jars :: [FilePath]
    , entryClass :: String
    , entryMethod :: String
    }

data Config
  = Config
    { common :: Czz.Conf.Type.Config
    , jvm :: JVMConfig
    }
