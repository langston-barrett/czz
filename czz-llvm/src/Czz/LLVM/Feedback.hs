module Czz.LLVM.Feedback
  ( Feedback(..)
  , empty
  , id
  )
where

import           Prelude hiding (id)
import           Data.ByteString (ByteString)
import qualified Data.Hashable as Hash
import           Data.Set (Set)
import qualified Data.Set as Set

import           Czz.Coverage (Coverage)
import qualified Czz.Coverage as Cover
import           Czz.KLimited (IsKLimited)

data Feedback k
  = Feedback
    { envVarsRead :: Set ByteString
    , filesOpened :: Set ByteString
    , coverage :: Coverage k
    }

empty :: IsKLimited k => Feedback k
empty =
  Feedback
  { envVarsRead = Set.empty
  , filesOpened = Set.empty
  , coverage = Cover.empty
  }

id :: Feedback k -> Int
id = Hash.hash . coverage
