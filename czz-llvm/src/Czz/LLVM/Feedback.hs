module Czz.LLVM.Feedback
  ( Feedback(..)
  , empty
  )
where

import           Prelude hiding (id)
import           Data.ByteString (ByteString)
import           Data.Set (Set)
import qualified Data.Set as Set

data Feedback
  = Feedback
    { envVarsRead :: Set ByteString
    , filesOpened :: Set ByteString
    }

empty :: Feedback
empty =
  Feedback
  { envVarsRead = Set.empty
  , filesOpened = Set.empty
  }
