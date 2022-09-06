{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Czz.Coverage.Path
  ( Path
  , empty
  , length
  , snoc
  )
where

import           Prelude hiding (length)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as AesonTH
import           Data.Hashable (Hashable)
import           GHC.Generics (Generic)

import           Czz.Coverage.BlockId (BlockId)
import           Czz.KLimited (KLimited, IsKLimited)
import qualified Czz.KLimited as KLimit

newtype Path k = Path { getPath :: KLimited k BlockId }
  deriving (Eq, Generic, Hashable, Ord, Show)

empty :: IsKLimited k => Path k
empty = Path KLimit.empty

length :: Path k -> Int
length = KLimit.length . getPath

snoc :: IsKLimited k => Path k -> BlockId -> Path k
snoc p x = Path (KLimit.snoc (getPath p) x)

$(AesonTH.deriveJSON
  Aeson.defaultOptions { Aeson.unwrapUnaryRecords = True }
  ''Path)
instance Aeson.ToJSONKey (Path k) where
instance Aeson.FromJSONKey (Path k) where
