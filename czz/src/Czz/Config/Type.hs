module Czz.Config.Type
  ( Config(..)
  , maxJobs
  , maxPathLen
  )
where

import qualified Data.Maybe as Maybe

-- TODO(lb): Configurable:
--
-- - Choice of solver
-- - Float mode
-- - Timeout
-- - Hash-consing
-- - Path strategy
-- - Verbosity
-- - etc.

data Config = Config
  { jobs :: !(Maybe Int)
  , pathLen :: !(Maybe Int)
  , seed :: !(Maybe Int)
  , tries :: !(Maybe Int)
  }

maxJobs :: Config -> Int
maxJobs = Maybe.fromMaybe 1 . jobs

maxPathLen :: Config -> Int
maxPathLen = Maybe.fromMaybe 2 . pathLen  -- 2 is edge coverage, like AFL
