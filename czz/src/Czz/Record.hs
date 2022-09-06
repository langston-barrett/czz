{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Czz.Record
  ( Record(..)
  , empty
  , hasBug
  , missing
  )
where

import           Prelude hiding (read)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as AesonTH

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)

import           Czz.SysTrace (Time(End))

import           Czz.Coverage.Seed (SeedCoverage)
import qualified Czz.Coverage.Seed as CSeed
import           Czz.KLimited (IsKLimited)
import           Czz.Result (Result)
import qualified Czz.Result as Res
import           Czz.Seed (Seed)
import qualified Czz.Seed as Seed

-- | A seed, along with data about its execution
data Record env eff k fb
  = Record
    { seed :: Seed 'End env eff
    , result :: Set Result
    , coverage :: SeedCoverage k
    , feedback :: fb
    }
  deriving (Eq, Functor, Ord)

empty :: IsKLimited k => env -> fb -> Record env eff k fb
empty env fb =
  Record
  { seed = Seed.end env
  , result = Set.empty
  , coverage = CSeed.empty
  , feedback = fb
  }

hasBug :: Record env eff k fb -> Bool
hasBug = any Res.isBug . Set.toList . result

missing :: Record env eff k fb -> Set Text
missing = Set.fromList . concatMap getMissing . Set.toList . result
  where
    getMissing =
      \case
        Res.MissingOverride t -> [t]
        _ -> []

$(AesonTH.deriveJSON Aeson.defaultOptions ''Record)
