{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Czz.Coverage.Seed
  ( SeedCoverage
  , empty
  , addPath
  , lastLoc
  , setLastLoc
  , freq
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as AesonTH
import           Data.Hashable (Hashable)
import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Czz.Coverage.Path (Path)
import           Czz.Freq (Freq)
import qualified Czz.Freq as Freq
import           Czz.KLimited (IsKLimited)

-- | Coverage achieved by a single execution/seed.
data SeedCoverage k =
  SeedCoverage
  { covFreq :: !(Freq (Path k))
  , covLastLoc :: !Text
  }
  deriving (Eq, Generic, Ord, Show)

instance Hashable (SeedCoverage k) where

empty :: IsKLimited k => SeedCoverage k
empty = SeedCoverage Freq.empty "<start>"

onPaths :: (Freq (Path k) -> Freq (Path k)) -> SeedCoverage k -> SeedCoverage k
onPaths f c = c { covFreq = f (covFreq c) }

addPath :: Path k -> SeedCoverage k -> SeedCoverage k
addPath p = onPaths (Freq.inc p)

lastLoc :: SeedCoverage k -> Text
lastLoc = covLastLoc

setLastLoc :: Text -> SeedCoverage k -> SeedCoverage k
setLastLoc l s = s { covLastLoc = l }

freq :: SeedCoverage k -> Freq (Path k)
freq = covFreq

$(AesonTH.deriveJSON Aeson.defaultOptions ''SeedCoverage)
