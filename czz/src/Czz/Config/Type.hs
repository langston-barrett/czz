module Czz.Config.Type
  ( Config(..)
  )
where

import           Czz.Log (Severity)

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
  { gas :: !(Maybe Int)
  , jobs :: !Int
  , pathLen :: !Int
  , seed :: !(Maybe Int)
  , tries :: !(Maybe Int)
  , verbosity :: !Severity
  }
