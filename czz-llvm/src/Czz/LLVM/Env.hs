{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Czz.LLVM.Env
  ( Env(..)
  , empty
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as AesonTH

import qualified Czz.LLVM.Env.Args as Args
import qualified Czz.LLVM.Env.FileSystem as FS

-- TODO(lb): doc
data Env
  = Env
    { args :: Args.Template
    , fs :: FS.Template
    }
  deriving (Eq, Ord)

empty :: Env
empty =
  Env
  { args = Args.empty
  , fs = FS.empty
  }

$(AesonTH.deriveJSON Aeson.defaultOptions ''Env)
