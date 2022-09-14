{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Scheme.From
  ( From(maybeFrom)
  , from
  , Opaque(..)
  -- * FromIO
  , FromIO
  , fromIO
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Except as Exc
import           Data.Array (Array)
import qualified Data.Dynamic as Dyn
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy (Proxy(Proxy))
import           Data.Typeable (Typeable)
import qualified Data.Typeable as Typ

import qualified Language.Scheme.Core as LSC
import qualified Language.Scheme.Types as LST

import           Language.Scheme.Opaque (Opaque(..))
import           Language.Scheme.To (To, to)

class From a where
  name :: proxy a -> String
  maybeFrom :: LST.LispVal -> Maybe a

from :: forall a. From a => LST.LispVal -> Either LST.LispError a
from v =
  maybe
    (Left (LST.TypeMismatch (name (Proxy @a)) v))
    Right
    (maybeFrom v)
{-# INLINABLE from #-}

instance From LST.LispVal where
  name _proxy = "lisp value"
  maybeFrom = Just
  {-# INLINABLE maybeFrom #-}

instance Typeable a => From (Opaque a) where
  name _proxy = "<Haskell " ++ show (Typ.typeRep (Proxy @a)) ++ ">"
  maybeFrom =
    \case
      LST.Opaque o -> Opaque <$> Dyn.fromDynamic o
      _ -> Nothing
  {-# INLINABLE maybeFrom #-}

instance From a => From (Array Int a) where
  name _proxy = unwords ["vector of" , name (Proxy @a)]
  maybeFrom =
    \case
      LST.Vector v -> traverse maybeFrom v
      _ -> Nothing
  {-# INLINABLE maybeFrom #-}

instance From Bool where
  name _proxy = "bool"
  maybeFrom =
    \case
      LST.Bool b -> Just b
      _ -> Nothing
  {-# INLINABLE maybeFrom #-}

instance From Char where
  name _proxy = "char"
  maybeFrom =
    \case
      LST.Char c -> Just c
      _ -> Nothing
  {-# INLINABLE maybeFrom #-}

instance From Double where
  name _proxy = "float"
  maybeFrom =
    \case
      LST.Float f -> Just f
      _ -> Nothing
  {-# INLINABLE maybeFrom #-}

instance From Integer where
  name _proxy = "number"
  maybeFrom =
    \case
      LST.Number i -> Just i
      _ -> Nothing
  {-# INLINABLE maybeFrom #-}

instance {-# OVERLAPPABLE #-} From a => From [a] where
  name _proxy = unwords ["list of" , name (Proxy @a)]
  maybeFrom =
    \case
      LST.List l -> traverse maybeFrom l
      _ -> Nothing
  {-# INLINABLE maybeFrom #-}

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
  {-# INLINABLE maybeFrom #-}

instance From String where
  name _proxy = "string"
  maybeFrom =
    \case
      LST.String s -> Just s
      _ -> Nothing
  {-# INLINABLE maybeFrom #-}

--------------------------------------------------------------------------------
-- FromIO

class FromIO a where
  nameIO :: proxy a -> String
  maybeFromIO :: LST.Env -> LST.LispVal -> Maybe (LST.IOThrowsError a)

  default nameIO :: From a => proxy a -> String
  nameIO = name

  default maybeFromIO :: From a => LST.Env -> LST.LispVal -> Maybe (LST.IOThrowsError a)
  maybeFromIO _env = fmap return . maybeFrom

fromIO_ :: forall a. FromIO a => LST.Env -> LST.LispVal -> IO (Either LST.LispError a)
fromIO_ env v = do
  case maybeFromIO env v of
    Nothing -> return (Left (LST.TypeMismatch (nameIO (Proxy @a)) v))
    Just comp -> Exc.runExceptT comp
{-# INLINABLE fromIO_ #-}

fromIO :: forall a. FromIO a => LST.LispVal -> IO (Either LST.LispError a)
fromIO v = do
  env <- liftIO LSC.r5rsEnv
  fromIO_ env v
{-# INLINABLE fromIO #-}

instance FromIO LST.LispVal where
instance Typeable a => FromIO (Opaque a) where
instance From a => FromIO (Array Int a) where
instance FromIO Bool where
instance FromIO Char where
instance FromIO Double where
instance FromIO Integer where
instance {-# OVERLAPPABLE #-} From a => FromIO [a] where
instance (Ord a , From a, From b) => FromIO (Map a b) where
instance FromIO String where

isFunc :: LST.LispVal -> Bool
isFunc =
  \case
    LST.CustFunc {} -> True
    LST.Func {} -> True
    LST.HFunc {} -> True
    LST.IOFunc {} -> True
    LST.PrimitiveFunc {} -> True
    _ -> False

instance (FromIO a, To a, FromIO b) => FromIO (a -> LST.IOThrowsError b) where
  nameIO _proxy = unwords [nameIO (Proxy @a), "->", nameIO (Proxy @b)]
  maybeFromIO env =
    \case
      f | isFunc f ->
       Just $ return $ \a -> do
         lispVal <- LSC.evalLisp env (LST.List [f, to a])
         Exc.ExceptT (fromIO_ env lispVal)
      _ -> Nothing
