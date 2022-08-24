{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}

module Czz.Record
  ( FeedbackId(..)
  , Record(..)
  , empty
  , hasBug
  )
where

import           Prelude hiding (read)

import           Data.Hashable (Hashable)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Czz.SysTrace (Time(End))

import           Czz.Result (Result)
import qualified Czz.Result as Res
import           Czz.Seed (Seed)
import qualified Czz.Seed as Seed

newtype FeedbackId = FeedbackId { getFeedbackId :: Int }
  deriving (Eq, Hashable, Ord, Show)

-- | A seed, along with data about its execution
data Record env eff fb
  = Record
    { seed :: Seed 'End env eff
    , result :: Set Result
    , feedback :: fb
    , feedbackId :: !FeedbackId
    }
  deriving (Eq, Functor, Ord)

empty :: env -> fb -> FeedbackId -> Record env eff fb
empty env fb fbId =
  Record
  { seed = Seed.end env
  , result = Set.empty
  , feedback = fb
  , feedbackId = fbId
  }

hasBug :: Record env eff fb -> Bool
hasBug = any Res.isBug . Set.toList . result
