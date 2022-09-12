{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Scheme.From
  ( From(maybeFrom)
  , from
  , Opaque(..)
  ) where

import           Data.Array (Array)
import qualified Data.Dynamic as Dyn
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy (Proxy(Proxy))
import           Data.Typeable (Typeable)
import qualified Data.Typeable as Typ

import qualified Language.Scheme.Types as LST

import           Language.Scheme.Opaque (Opaque(..))

class From a where
  name :: proxy a -> String
  maybeFrom :: LST.LispVal -> Maybe a

from :: forall a. From a => LST.LispVal -> Either LST.LispError a
from v =
  maybe
    (Left (LST.TypeMismatch (name (Proxy :: Proxy a)) v))
    Right
    (maybeFrom v)

instance Typeable a => From (Opaque a) where
  name _proxy = show (Typ.typeRep (Proxy @a))
  maybeFrom =
    \case
      LST.Opaque o -> Opaque <$> Dyn.fromDynamic o
      _ -> Nothing

instance From a => From (Array Int a) where
  name _proxy = unwords ["vector of" , name (Proxy @a)]
  maybeFrom =
    \case
      LST.Vector v -> traverse maybeFrom v
      _ -> Nothing

instance From Bool where
  name _proxy = "bool"
  maybeFrom =
    \case
      LST.Bool b -> Just b
      _ -> Nothing

instance From Char where
  name _proxy = "char"
  maybeFrom =
    \case
      LST.Char c -> Just c
      _ -> Nothing

instance From Double where
  name _proxy = "float"
  maybeFrom =
    \case
      LST.Float f -> Just f
      _ -> Nothing

instance From Integer where
  name _proxy = "number"
  maybeFrom =
    \case
      LST.Number i -> Just i
      _ -> Nothing

instance {-# OVERLAPPABLE #-} From a => From [a] where
  name _proxy = unwords ["list of" , name (Proxy @a)]
  maybeFrom =
    \case
      LST.List l -> traverse maybeFrom l
      _ -> Nothing

instance
  ( Ord a
  , From a
  , From b
  ) => From (Map a b) where
  name _proxy =
    unwords
      [ "map from"
      , name (Proxy @a)
      , "to"
      , name (Proxy @b)
      ]

  maybeFrom =
    \case
      LST.HashTable m ->
        Map.fromList <$>
          traverse
            (\(k, v) -> (,) <$> maybeFrom k <*> maybeFrom v)
            (Map.toList m)
      _ -> Nothing

instance From String where
  name _proxy = "string"
  maybeFrom =
    \case
      LST.String s -> Just s
      _ -> Nothing
