module Czz.Config.Type
  ( Config(..)
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

data Config = Config
  { bucketing :: !BucketingName
  , gas :: !(Maybe Int)
  , jobs :: !Int
  , pathLen :: !Int
  , seed :: !(Maybe Int)
  , stateDir :: !(Maybe FilePath)
  , tries :: !(Maybe Int)
  , verbosity :: !Severity
  }
