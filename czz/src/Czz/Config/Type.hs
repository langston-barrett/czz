module Czz.Config.Type
  ( Config(..)
  )
where

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
  { jobs :: !Int
  , pathLen :: !Int
  , seed :: !(Maybe Int)
  , tries :: !(Maybe Int)
  }
