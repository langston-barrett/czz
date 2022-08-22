module Czz.JVM.Config.Type
  ( Config(..)
  )
where

import qualified Czz.Config.Type as Czz.Conf.Type

data Config = Config
  { common :: !Czz.Conf.Type.Config
  , classPath :: ![FilePath]
  , jars :: ![FilePath]
  , entryClass :: !(Maybe String)
  , entryMethod :: !(Maybe String)
  }
