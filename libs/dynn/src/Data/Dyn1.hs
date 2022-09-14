{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Dyn1
  ( Dyn1
  , to
  , some
  , from
  , view
  , map
  ) where

import           Prelude hiding (map)
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

to :: forall f a. Typeable f => Typeable a => f a -> Dyn1 f
to fa =
  Dyn1
  { inner = Reflect.typeRep @a
  , val = fa
  }

some :: forall f. Dyn1 f -> Some f
some (Dyn1 _i v) = Some v

from :: forall f. Typeable f => Functor f => Dyn1 f -> f Dynamic
from (Dyn1 i v) = Reflect.withTypeable i (toDyn <$> v)

view :: (forall a. f a -> b) -> Dyn1 f -> b
view k (Dyn1 _i v) = k v

map :: forall f b. Typeable b => (forall a. f a -> f b) -> Dyn1 f -> Dyn1 f
map f (Dyn1 _i v) = Dyn1 (Reflect.typeRep @b) (f v)
