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
import qualified Language.Scheme.Variables as LSV

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
  name _proxy = concat ["(" , name (Proxy @a), ")"]
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

instance
  ( From a
  , From b
  ) => From (a, b) where
  name _proxy = concat ["(", name (Proxy @a), " ", name (Proxy @b), ")"]
  maybeFrom =
    \case
      LST.List [a, b] -> (,) <$> maybeFrom a <*> maybeFrom b
      _ -> Nothing
  {-# INLINABLE maybeFrom #-}

instance
  ( From a
  , From b
  , From c
  ) => From (a, b, c) where
  name _proxy =
    concat
      [ "("
      , name (Proxy @a)
      , " "
      , name (Proxy @b)
      , " "
      , name (Proxy @c)
      , ")"
      ]
  maybeFrom =
    \case
      LST.List [a, b, c] ->
        (,,)
        <$> maybeFrom a
        <*> maybeFrom b
        <*> maybeFrom c
      _ -> Nothing
  {-# INLINABLE maybeFrom #-}

instance
  ( From a
  , From b
  , From c
  , From d
  ) => From (a, b, c, d) where
  name _proxy =
    concat
      [ "("
      , name (Proxy @a)
      , " "
      , name (Proxy @b)
      , " "
      , name (Proxy @c)
      , " "
      , name (Proxy @d)
      , ")"
      ]
  maybeFrom =
    \case
      LST.List [a, b, c, d] ->
        (,,,)
        <$> maybeFrom a
        <*> maybeFrom b
        <*> maybeFrom c
        <*> maybeFrom d
      _ -> Nothing
  {-# INLINABLE maybeFrom #-}

instance
  ( From a
  , From b
  , From c
  , From d
  , From e
  ) => From (a, b, c, d, e) where
  name _proxy =
    concat
      [ "("
      , name (Proxy @a)
      , " "
      , name (Proxy @b)
      , " "
      , name (Proxy @c)
      , " "
      , name (Proxy @d)
      , " "
      , name (Proxy @e)
      , ")"
      ]
  maybeFrom =
    \case
      LST.List [a, b, c, d, e] ->
        (,,,,)
        <$> maybeFrom a
        <*> maybeFrom b
        <*> maybeFrom c
        <*> maybeFrom d
        <*> maybeFrom e
      _ -> Nothing
  {-# INLINABLE maybeFrom #-}

--------------------------------------------------------------------------------
-- FromIO

noPtrs :: LST.LispVal -> LST.IOThrowsError LST.LispVal
noPtrs =
  \case
    x@LST.Atom {} -> return x
    x@LST.Bool {} -> return x
    x@LST.ByteVector {} -> return x
    x@LST.Char {} -> return x
    x@LST.Complex {} -> return x
    x@LST.EOF {} -> return x
    x@LST.Float {} -> return x
    x@LST.LispEnv {} -> return x
    x@LST.Nil {} -> return x
    x@LST.Number {} -> return x
    x@LST.Opaque {} -> return x
    x@LST.Port {} -> return x
    x@LST.Rational {} -> return x
    x@LST.String {} -> return x
    --
    p@LST.Pointer {} -> LSV.derefPtr p
    LST.HashTable t ->
      LST.HashTable . Map.fromList <$>
        mapM (\(k, v) -> (,) <$> noPtrs k <*> noPtrs v) (Map.toList t)
    LST.List l -> LST.List <$> traverse noPtrs l
    LST.DottedList l a -> LST.DottedList <$> traverse noPtrs l <*> noPtrs a
    LST.Vector v -> LST.Vector <$> traverse noPtrs v
    v -> return v

class FromIO a where
  nameIO :: proxy a -> String
  fromIO_ :: LST.Env -> LST.LispVal -> LST.IOThrowsError a

  default nameIO :: From a => proxy a -> String
  nameIO = name

  default fromIO_ :: From a => LST.Env -> LST.LispVal -> LST.IOThrowsError a
  fromIO_ _env v = Exc.ExceptT . return . from =<< noPtrs v

fromIO :: forall a. FromIO a => LST.LispVal -> IO (Either LST.LispError a)
fromIO v = do
  env <- liftIO LSC.r5rsEnv
  Exc.runExceptT (fromIO_ env v)
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
instance (From a, From b) => FromIO (a, b) where
instance (From a, From b, From c) => FromIO (a, b, c) where
instance (From a, From b, From c, From d) => FromIO (a, b, c, d) where
instance (From a, From b, From c, From d, From e) => FromIO (a, b, c, d, e) where
instance FromIO String where

-- | Helper, not exported
isFunc :: LST.LispVal -> Bool
isFunc =
  \case
    LST.CustFunc {} -> True
    LST.Func {} -> True
    LST.HFunc {} -> True
    LST.IOFunc {} -> True
    LST.PrimitiveFunc {} -> True
    _ -> False

-- | Helper, not exported
typeError :: FromIO a => proxy a -> LST.LispVal -> LST.IOThrowsError b
typeError proxy v =
  Exc.ExceptT (return (Left (LST.TypeMismatch (nameIO proxy) v)))

instance (FromIO a, To a, FromIO b) => FromIO (a -> LST.IOThrowsError b) where
  nameIO _proxy = unwords [nameIO (Proxy @a), "->", nameIO (Proxy @b)]
  fromIO_ env =
    \case
      f | isFunc f ->
        return $
          \a -> do
            lispVal <- LSC.apply f f [to a]
            fromIO_ env lispVal
      v -> typeError (Proxy @(a -> LST.IOThrowsError b)) v

instance
  ( FromIO a
  , FromIO b
  , To a
  , To b
  , FromIO c
  ) => FromIO (a -> b -> LST.IOThrowsError c) where
  nameIO _proxy =
    unwords
      [ nameIO (Proxy @a)
      , "->"
      , nameIO (Proxy @b)
      , "->"
      , nameIO (Proxy @c)
      ]
  fromIO_ env =
    \case
      f | isFunc f ->
       return $
         \a b -> do
           lispVal <- LSC.apply f f [to a, to b]
           fromIO_ env lispVal
      v -> typeError (Proxy @(a -> b -> LST.IOThrowsError c)) v

instance
  ( FromIO a
  , FromIO b
  , FromIO c
  , To a
  , To b
  , To c
  , FromIO d
  ) => FromIO (a -> b -> c -> LST.IOThrowsError d) where
  nameIO _proxy =
    unwords
      [ nameIO (Proxy @a)
      , "->"
      , nameIO (Proxy @b)
      , "->"
      , nameIO (Proxy @c)
      , "->"
      , nameIO (Proxy @d)
      ]
  fromIO_ env =
    \case
      f | isFunc f ->
       return $
         \a b c -> do
           lispVal <- LSC.apply f f [to a, to b, to c]
           fromIO_ env lispVal
      v -> typeError (Proxy @(a -> b -> c -> LST.IOThrowsError d)) v

instance
  ( FromIO a
  , FromIO b
  , FromIO c
  , FromIO d
  , To a
  , To b
  , To c
  , To d
  , FromIO e
  ) => FromIO (a -> b -> c -> d -> LST.IOThrowsError e) where
  nameIO _proxy =
    unwords
      [ nameIO (Proxy @a)
      , "->"
      , nameIO (Proxy @b)
      , "->"
      , nameIO (Proxy @c)
      , "->"
      , nameIO (Proxy @d)
      , "->"
      , nameIO (Proxy @e)
      ]
  fromIO_ env =
    \case
      f | isFunc f ->
       return $
         \a b c d -> do
           lispVal <- LSC.apply f f [to a, to b, to c, to d]
           fromIO_ env lispVal
      v -> typeError (Proxy @(a -> b -> c -> d -> LST.IOThrowsError e)) v
