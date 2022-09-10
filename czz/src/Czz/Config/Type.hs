{-# LANGUAGE StrictData #-}

module Czz.Config.Type
  ( FuzzConfig(..)
  , ReplayConfig(..)
  , Cmd(..)
  , BaseConfig(..)
  , Config(..)
  )
where

import           Czz.Coverage.Bucket.Bucketing (BucketingName)
import           Czz.Log (Severity)

-- TODO(lb): Configurable:
--
-- - Choice of solver
-- - Float mode
-- - Timeout
-- - Hash-consing
-- - Path strategy
-- - etc.

data FuzzConfig
  = FuzzConfig
    { bucketing :: BucketingName
    , gas :: Maybe Int
    , jobs :: Int
    , pathLen :: Int
    , seed :: Maybe Int
    , stateDir :: Maybe FilePath
    , tries :: Maybe Int
    }

data ReplayConfig
  = ReplayConfig
    { seedFile :: FilePath
    }

data Cmd
  = CmdFuzz FuzzConfig
  | CmdReplay ReplayConfig

-- | Shared between all subcommands
data BaseConfig = BaseConfig
  { verbosity :: Severity
  }

data Config = Config
  { base :: BaseConfig
  , command :: Cmd
  }
