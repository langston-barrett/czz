-- TODO(lb): This should be in its own library, along with Dyn2, ...

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Scheme.Dyn1
  ( Dyn1
  , toDyn1
  , someDyn1
  , fromDyn1
  , viewDyn1
  , mapDyn1
  ) where

import           Data.Dynamic (Dynamic, toDyn)
import           Type.Reflection (Typeable, TypeRep)
import qualified Type.Reflection as Reflect

import           Data.Parameterized.Some (Some(Some))

data Dyn1 f
  = forall a.
    Dyn1
    { inner :: TypeRep a
    , val :: f a
    }

toDyn1 :: forall f a. Typeable f => Typeable a => f a -> Dyn1 f
toDyn1 fa =
  Dyn1
  { inner = Reflect.typeRep @a
  , val = fa
  }

someDyn1 :: forall f. Dyn1 f -> Some f
someDyn1 (Dyn1 _i v) = Some v

fromDyn1 :: forall f. Typeable f => Functor f => Dyn1 f -> f Dynamic
fromDyn1 (Dyn1 i v) = Reflect.withTypeable i (toDyn <$> v)

viewDyn1 :: (forall a. f a -> b) -> Dyn1 f -> b
viewDyn1 k (Dyn1 _i v) = k v

mapDyn1 :: forall f b. Typeable b => (forall a. f a -> f b) -> Dyn1 f -> Dyn1 f
mapDyn1 f (Dyn1 _i v) = Dyn1 (Reflect.typeRep @b) (f v)
