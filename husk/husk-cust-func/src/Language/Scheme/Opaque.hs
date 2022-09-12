module Language.Scheme.Opaque
  ( Opaque(..)
  ) where

-- | Useful in conjunction with 'Data.Coerce.coerce'.
newtype Opaque a = Opaque { getOpaque :: a }
  deriving Show
