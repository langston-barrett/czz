{-# LANGUAGE FlexibleInstances #-}

module Language.Scheme.To
  ( To
  , to
  ) where

import           Data.Array (Array)
import qualified Data.Dynamic as Dyn
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Typeable (Typeable)

import qualified Language.Scheme.Types as LST

import           Language.Scheme.Opaque (Opaque(getOpaque))

class To a where
  to :: a -> LST.LispVal

instance To LST.LispVal where
  to = id

instance Typeable a => To (Opaque a) where
  to = LST.Opaque . Dyn.toDyn . getOpaque

instance To a => To (Array Int a) where
  to = LST.Vector . fmap to

instance To Bool where
  to = LST.Bool

instance To Char where
  to = LST.Char

instance To Double where
  to = LST.Float

instance To Integer where
  to = LST.Number

instance {-# OVERLAPPABLE #-} To a => To [a] where
  to = LST.List . fmap to

instance
  ( Ord a
  , To a
  , To b
  ) => To (Map a b) where
  to = LST.HashTable . Map.mapKeys to . Map.map to

instance To String where
  to = LST.String
