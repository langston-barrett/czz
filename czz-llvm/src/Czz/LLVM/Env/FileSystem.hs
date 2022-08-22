-- TODO(lb): directly wrap symio initial fs type?
-- TODO(lb): export higher-level operations
-- TODO(lb): start empty

{-# LANGUAGE OverloadedStrings #-}

module Czz.LLVM.Env.FileSystem
  ( Template
  , empty
  , nullFs
  , setStdin
  , addFile
  , rmFile
  , paths
  , fsMap
  )
where

import           Data.ByteString (ByteString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Template
  = Template
      { fs :: Map ByteString ByteString
      , stdin :: ByteString
      }
  deriving (Eq, Ord, Show)

empty :: Template
empty =
  Template
    { fs = Map.empty
    , stdin = ""
    }

nullFs :: Template -> Bool
nullFs = Map.null . fs

setStdin :: ByteString -> Template -> Template
setStdin s t = t { stdin = s }

addFile :: ByteString -> ByteString -> Template -> Template
addFile path content t = t { fs = Map.insert path content (fs t) }

rmFile :: ByteString -> Template -> Template
rmFile path t = t { fs = Map.delete path (fs t) }

paths :: Template -> [ByteString]
paths = Map.keys . fs

fsMap :: Template -> Map ByteString ByteString
fsMap = fs
