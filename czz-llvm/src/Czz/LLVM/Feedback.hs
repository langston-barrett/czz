{-# LANGUAGE TemplateHaskell #-}

module Czz.LLVM.Feedback
  ( Feedback(..)
  , empty
  )
where

import           Prelude hiding (id)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as AesonTH
import           Data.ByteString (ByteString)
import           Data.Set (Set)
import qualified Data.Set as Set

import qualified Czz.Orphans ()

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

$(AesonTH.deriveJSON Aeson.defaultOptions ''Feedback)
