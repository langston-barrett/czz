module Czz.LLVM.Config.Type
  ( Config(..)
  )
where

import qualified Czz.Config.Type as Czz.Conf.Type

data Config = Config
  { common :: !Czz.Conf.Type.Config
  , prog :: !FilePath
  , entryPoint :: !(Maybe String)
  , skip :: ![String]
  , onlyNeeded :: !Bool
  }
