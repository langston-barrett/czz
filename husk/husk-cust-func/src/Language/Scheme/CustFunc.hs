{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Scheme.CustFunc
  ( Huskable
  , evalHuskable
  -- * Opaque
  , opaque0
  , opaque1
  , opaque2
  , opaque3
  , opaque4
  , opaque5
  , opaqueArgs1
  , opaqueArgs2
  , opaqueArgs3
  , opaqueArgs4
  , opaqueArgs5
  , opaqueRet1
  , opaqueRet2
  , opaqueRet3
  , opaqueRet4
  , opaqueRet5
  -- * Auto
  , Auto
  , auto
  ) where

import qualified Control.Monad.Except as Exc
import           Data.Array (Array)
import           Data.Coerce (Coercible, coerce)
import           Data.Map (Map)
import           Data.Typeable (Typeable)

import qualified Language.Scheme.Types as LST

import           Language.Scheme.From (From, Opaque)
import qualified Language.Scheme.From as From
import           Language.Scheme.To (To(to))

class Huskable a where
  evalHuskable :: a -> [LST.LispVal] -> LST.IOThrowsError LST.LispVal

instance Huskable a => Huskable (LST.IOThrowsError a) where
  evalHuskable comp as = do
    x <- comp
    evalHuskable x as
  {-# INLINABLE evalHuskable #-}

instance Huskable LST.LispVal where
  evalHuskable x as =
    if null as
    then return x 
    else Exc.throwError (LST.NumArgs Nothing as)
  {-# INLINABLE evalHuskable #-}

instance Typeable a => Huskable (Opaque a) where
  evalHuskable x = evalHuskable (to x)
  {-# INLINABLE evalHuskable #-}

instance To a => Huskable (Array Int a) where
  evalHuskable x = evalHuskable (to x)
  {-# INLINABLE evalHuskable #-}

instance Huskable Bool where
  evalHuskable x = evalHuskable (to x)
  {-# INLINABLE evalHuskable #-}

instance Huskable Char where
  evalHuskable x = evalHuskable (to x)
  {-# INLINABLE evalHuskable #-}

instance Huskable Double where
  evalHuskable x = evalHuskable (to x)
  {-# INLINABLE evalHuskable #-}

instance Huskable Integer where
  evalHuskable x = evalHuskable (to x)
  {-# INLINABLE evalHuskable #-}

instance {-# OVERLAPPABLE #-} To a => Huskable [a] where
  evalHuskable x = evalHuskable (to x)
  {-# INLINABLE evalHuskable #-}

instance
  ( Ord a
  , To a
  , To b
  ) => Huskable (Map a b) where
  evalHuskable x = evalHuskable (to x)
  {-# INLINABLE evalHuskable #-}

instance Huskable String where
  evalHuskable x = evalHuskable (to x)
  {-# INLINABLE evalHuskable #-}

instance
  ( From a
  , Huskable b
  ) => Huskable (a -> b) where
  evalHuskable f as =
    case as of
      [] -> Exc.throwError (LST.NumArgs Nothing as)
      (x:xs) -> do
        x' <- Exc.ExceptT (return (From.from x))
        evalHuskable (f x') xs
  {-# INLINABLE evalHuskable #-}

--------------------------------------------------------------------------------
-- Opaque

opaque0 :: a -> Opaque a
opaque0 = coerce
{-# INLINE opaque0 #-}

opaque1 ::
  (a -> b) ->
  Opaque a ->
  Opaque b
opaque1 = coerce
{-# INLINE opaque1 #-}

opaque2 ::
  (a -> b -> c) ->
  Opaque a ->
  Opaque b ->
  Opaque c
opaque2 = coerce
{-# INLINE opaque2 #-}

opaque3 ::
  (a -> b -> c -> d) ->
  Opaque a ->
  Opaque b ->
  Opaque c ->
  Opaque d
opaque3 = coerce
{-# INLINE opaque3 #-}

opaque4 ::
  (a -> b -> c -> d -> e) ->
  Opaque a ->
  Opaque b ->
  Opaque c ->
  Opaque d ->
  Opaque e
opaque4 = coerce
{-# INLINE opaque4 #-}

opaque5 ::
  (a -> b -> c -> d -> e -> f) ->
  Opaque a ->
  Opaque b ->
  Opaque c ->
  Opaque d ->
  Opaque e ->
  Opaque f
opaque5 = coerce
{-# INLINE opaque5 #-}

opaqueArgs1 ::
  (a -> b) ->
  Opaque a ->
  b
opaqueArgs1 = coerce
{-# INLINE opaqueArgs1 #-}

opaqueArgs2 ::
  (a -> b -> c) ->
  Opaque a ->
  Opaque b ->
  c
opaqueArgs2 = coerce
{-# INLINE opaqueArgs2 #-}

opaqueArgs3 ::
  (a -> b -> c -> d) ->
  Opaque a ->
  Opaque b ->
  Opaque c ->
  d
opaqueArgs3 = coerce
{-# INLINE opaqueArgs3 #-}

opaqueArgs4 ::
  (a -> b -> c -> d -> e) ->
  Opaque a ->
  Opaque b ->
  Opaque c ->
  Opaque d ->
  e
opaqueArgs4 = coerce
{-# INLINE opaqueArgs4 #-}

opaqueArgs5 ::
  (a -> b -> c -> d -> e -> f) ->
  Opaque a ->
  Opaque b ->
  Opaque c ->
  Opaque d ->
  Opaque e ->
  f
opaqueArgs5 = coerce
{-# INLINE opaqueArgs5 #-}

opaqueRet1 ::
  (a -> b) ->
  a ->
  Opaque b
opaqueRet1 = coerce
{-# INLINE opaqueRet1 #-}

opaqueRet2 ::
  (a -> b -> c) ->
  a ->
  b ->
  Opaque c
opaqueRet2 = coerce
{-# INLINE opaqueRet2 #-}

opaqueRet3 ::
  (a -> b -> c -> d) ->
  a ->
  b ->
  c ->
  Opaque d
opaqueRet3 = coerce
{-# INLINE opaqueRet3 #-}

opaqueRet4 ::
  (a -> b -> c -> d -> e) ->
  a ->
  b ->
  c ->
  d ->
  Opaque e
opaqueRet4 = coerce
{-# INLINE opaqueRet4 #-}

opaqueRet5 ::
  (a -> b -> c -> d -> e -> f) ->
  a ->
  b ->
  c ->
  d ->
  e ->
  Opaque f
opaqueRet5 = coerce
{-# INLINE opaqueRet5 #-}

--------------------------------------------------------------------------------
-- Auto

type family Auto a where
  Auto LST.LispVal = LST.LispVal
  Auto (Array i a) = Array i a
  Auto Bool = Bool
  Auto Char = Char
  Auto Double = Double
  Auto Integer = Integer
  Auto [a] = [a]
  Auto (Map a b) = Map a b
  Auto (a -> b) = Auto a -> Auto b
  Auto a = Opaque a

class Coercible a (Auto a) => Autoable a where
  auto_ :: a -> Auto a
  auto_ = coerce
  {-# INLINE auto_ #-}

instance Coercible a (Auto a) => Autoable a where

-- | 'auto can usually figure out which arguments and return values should be
-- wrapped in 'Opaque'.
auto :: Coercible a (Auto a) => a -> Auto a
auto = auto_
{-# INLINE auto #-}
