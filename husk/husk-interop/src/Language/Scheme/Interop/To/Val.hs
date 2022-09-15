{-# LANGUAGE FlexibleInstances #-}

module Language.Scheme.Interop.To.Val
  ( ToSchemeVal
  , toSchemeVal
  ) where

import           Data.Array (Array)
import qualified Data.Dynamic as Dyn
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Typeable (Typeable)

import qualified Language.Scheme.Types as LST

import           Language.Scheme.Interop.Opaque (Opaque(getOpaque))

class ToSchemeVal a where
  toSchemeVal :: a -> LST.LispVal

instance ToSchemeVal LST.LispVal where
  toSchemeVal = id
  {-# INLINE toSchemeVal #-}

instance Typeable a => ToSchemeVal (Opaque a) where
  toSchemeVal = LST.Opaque . Dyn.toDyn . getOpaque
  {-# INLINE toSchemeVal #-}

instance ToSchemeVal a => ToSchemeVal (Array Int a) where
  toSchemeVal = LST.Vector . fmap toSchemeVal
  {-# INLINE toSchemeVal #-}

instance ToSchemeVal Bool where
  toSchemeVal = LST.Bool
  {-# INLINE toSchemeVal #-}

instance ToSchemeVal Char where
  toSchemeVal = LST.Char
  {-# INLINE toSchemeVal #-}

instance ToSchemeVal Double where
  toSchemeVal = LST.Float
  {-# INLINE toSchemeVal #-}

instance ToSchemeVal Integer where
  toSchemeVal = LST.Number
  {-# INLINE toSchemeVal #-}

instance {-# OVERLAPPABLE #-} ToSchemeVal a => ToSchemeVal [a] where
  toSchemeVal = LST.List . fmap toSchemeVal
  {-# INLINE toSchemeVal #-}

-- | Maps are encoded as hash tables.
instance
  ( Ord a
  , ToSchemeVal a
  , ToSchemeVal b
  ) => ToSchemeVal (Map a b) where
  toSchemeVal = LST.HashTable . Map.mapKeys toSchemeVal . Map.map toSchemeVal
  {-# INLINE toSchemeVal #-}

instance ToSchemeVal String where
  toSchemeVal = LST.String
  {-# INLINE toSchemeVal #-}

-- | Tuples are encoded as lists.
instance
  ( ToSchemeVal a
  , ToSchemeVal b
  ) => ToSchemeVal (a, b) where
  toSchemeVal (a, b) = LST.List [toSchemeVal a, toSchemeVal b]
  {-# INLINE toSchemeVal #-}

-- | Tuples are encoded as lists.
instance
  ( ToSchemeVal a
  , ToSchemeVal b
  , ToSchemeVal c
  ) => ToSchemeVal (a, b, c) where
  toSchemeVal (a, b, c) = LST.List [toSchemeVal a, toSchemeVal b, toSchemeVal c]
  {-# INLINE toSchemeVal #-}

-- | Tuples are encoded as lists.
instance
  ( ToSchemeVal a
  , ToSchemeVal b
  , ToSchemeVal c
  , ToSchemeVal d
  ) => ToSchemeVal (a, b, c, d) where
  toSchemeVal (a, b, c, d) =
    LST.List [toSchemeVal a, toSchemeVal b, toSchemeVal c, toSchemeVal d]
  {-# INLINE toSchemeVal #-}

-- | Tuples are encoded as lists.
instance
  ( ToSchemeVal a
  , ToSchemeVal b
  , ToSchemeVal c
  , ToSchemeVal d
  , ToSchemeVal e
  ) => ToSchemeVal (a, b, c, d, e) where
  toSchemeVal (a, b, c, d, e) =
    LST.List [toSchemeVal a, toSchemeVal b, toSchemeVal c, toSchemeVal d, toSchemeVal e]
  {-# INLINE toSchemeVal #-}
