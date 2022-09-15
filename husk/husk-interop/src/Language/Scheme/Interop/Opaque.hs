module Language.Scheme.Interop.Opaque
  ( Opaque(..)
  , Opaque1(..)
  ) where

import           Data.Dynamic (Dynamic)

-- | Values that are marked as 'Opaque' in function signatures are wrapped and
-- unwrapped from 'Language.Scheme.Types.Opaque' (and 'Data.Dynamic.Dynamic').
--
-- Intended to be used in conjunction with 'Data.Coerce.coerce'.
newtype Opaque a = Opaque { getOpaque :: a }
  deriving Show

newtype Opaque1 f = Opaque1 { getOpaque1 :: f Dynamic }
