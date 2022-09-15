{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Scheme.Interop.To.Func.Auto
  ( Auto
  , auto
  , auto1
  ) where

import           Data.Array (Array)
import           Data.Coerce (Coercible, coerce)
import           Data.Map (Map)

import qualified Language.Scheme.Types as LST

import           Language.Scheme.Interop.Opaque (Opaque)
import           Language.Scheme.Interop.To.Func (Ret(..))

type family Auto a where
  Auto (LST.IOThrowsError a) = LST.IOThrowsError (Auto a)
  -- This won't be able to match with Coercible, so it will raise an error.
  -- You should use LST.IOThrowsError instead. Catches silly mistakes that would
  -- end up turning IO computations opaque, instead of running them.
  Auto (IO a) = ()

  Auto LST.LispVal = LST.LispVal
  Auto (Array i a) = Array i a
  Auto Bool = Bool
  Auto Char = Char
  Auto Double = Double
  Auto Integer = Integer
  Auto [a] = [Auto a]
  Auto (Map a b) = Map a b

  Auto (a, b) = (Auto a, Auto b)
  Auto (a, b, c) = (Auto a, Auto b, Auto c)
  Auto (a, b, c, d) = (Auto a, Auto b, Auto c, Auto d)
  Auto (a, b, c, d, e) = (Auto a, Auto b, Auto c, Auto d, Auto e)

  Auto (a -> b -> c -> LST.IOThrowsError d) = Auto a -> Auto b -> Auto c -> LST.IOThrowsError (Auto d)
  Auto (a -> b -> LST.IOThrowsError c) = Auto a -> Auto b -> LST.IOThrowsError (Auto c)
  Auto (a -> LST.IOThrowsError b) = Auto a -> LST.IOThrowsError (Auto b)

  Auto (a -> b -> c -> d) = Auto a -> Auto b -> Auto c -> Ret (Auto d)
  Auto (a -> b -> c) = Auto a -> Auto b -> Ret (Auto c)
  Auto (a -> b) = Auto a -> Ret (Auto b)

  Auto a = Opaque a

-- | 'auto' can usually figure out which arguments and return values should be
-- wrapped in 'Opaque' and 'Ret'.
auto :: Coercible a (Auto a) => a -> Auto a
auto = coerce
{-# INLINE auto #-}

-- TODO(lb): rename to auto0
auto1 :: Coercible a (Auto a) => a -> Ret (Auto a)
auto1 = coerce
{-# INLINE auto1 #-}
