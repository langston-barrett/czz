{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Scheme.CustFunc
  ( Huskable
  , evalHuskable
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
  ) where

import qualified Control.Monad.Except as Exc
import           Data.Array (Array)
import           Data.Coerce (coerce)
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

instance Huskable LST.LispVal where
  evalHuskable x as =
    if length as /= 0
    then Exc.throwError (LST.NumArgs Nothing as)
    else return x

instance Typeable a => Huskable (Opaque a) where
  evalHuskable x = evalHuskable (to x)

instance To a => Huskable (Array Int a) where
  evalHuskable x = evalHuskable (to x)

instance Huskable Bool where
  evalHuskable x = evalHuskable (to x)

instance Huskable Char where
  evalHuskable x = evalHuskable (to x)

instance Huskable Double where
  evalHuskable x = evalHuskable (to x)

instance Huskable Integer where
  evalHuskable x = evalHuskable (to x)

instance {-# OVERLAPPABLE #-} To a => Huskable [a] where
  evalHuskable x = evalHuskable (to x)

instance
  ( Ord a
  , To a
  , To b
  ) => Huskable (Map a b) where
  evalHuskable x = evalHuskable (to x)

instance Huskable String where
  evalHuskable x = evalHuskable (to x)

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

opaque0 :: a -> Opaque a
opaque0 = coerce

opaque1 ::
  (a -> b) ->
  Opaque a ->
  Opaque b
opaque1 = coerce

opaque2 ::
  (a -> b -> c) ->
  Opaque a ->
  Opaque b ->
  Opaque c
opaque2 = coerce

opaque3 ::
  (a -> b -> c -> d) ->
  Opaque a ->
  Opaque b ->
  Opaque c ->
  Opaque d
opaque3 = coerce

opaque4 ::
  (a -> b -> c -> d -> e) ->
  Opaque a ->
  Opaque b ->
  Opaque c ->
  Opaque d ->
  Opaque e
opaque4 = coerce

opaque5 ::
  (a -> b -> c -> d -> e -> f) ->
  Opaque a ->
  Opaque b ->
  Opaque c ->
  Opaque d ->
  Opaque e ->
  Opaque f
opaque5 = coerce

opaqueArgs1 ::
  (a -> b) ->
  Opaque a ->
  b
opaqueArgs1 = coerce

opaqueArgs2 ::
  (a -> b -> c) ->
  Opaque a ->
  Opaque b ->
  c
opaqueArgs2 = coerce

opaqueArgs3 ::
  (a -> b -> c -> d) ->
  Opaque a ->
  Opaque b ->
  Opaque c ->
  d
opaqueArgs3 = coerce

opaqueArgs4 ::
  (a -> b -> c -> d -> e) ->
  Opaque a ->
  Opaque b ->
  Opaque c ->
  Opaque d ->
  e
opaqueArgs4 = coerce

opaqueArgs5 ::
  (a -> b -> c -> d -> e -> f) ->
  Opaque a ->
  Opaque b ->
  Opaque c ->
  Opaque d ->
  Opaque e ->
  f
opaqueArgs5 = coerce

opaqueRet1 ::
  (a -> b) ->
  a ->
  Opaque b
opaqueRet1 = coerce

opaqueRet2 ::
  (a -> b -> c) ->
  a ->
  b ->
  Opaque c
opaqueRet2 = coerce

opaqueRet3 ::
  (a -> b -> c -> d) ->
  a ->
  b ->
  c ->
  Opaque d
opaqueRet3 = coerce

opaqueRet4 ::
  (a -> b -> c -> d -> e) ->
  a ->
  b ->
  c ->
  d ->
  Opaque e
opaqueRet4 = coerce

opaqueRet5 ::
  (a -> b -> c -> d -> e -> f) ->
  a ->
  b ->
  c ->
  d ->
  e ->
  Opaque f
opaqueRet5 = coerce
