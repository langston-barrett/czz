{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Scheme.Interop.From
  ( FromScheme
  , fromScheme
  ) where

import qualified Control.Monad.Except as Exc
import           Data.Array (Array)
import qualified Data.Dynamic as Dyn
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy (Proxy(Proxy))
import           Data.Typeable (Typeable)

import qualified Language.Scheme.Core as LSC
import qualified Language.Scheme.Types as LST
import qualified Language.Scheme.Variables as LSV

import           Language.Scheme.Interop.Opaque (Opaque(..))
import           Language.Scheme.Interop.To.Val
import           Language.Scheme.Interop.TypeName (SchemeTypeName, schemeTypeName)

class SchemeTypeName a => FromScheme a where
  fromScheme_ :: LST.LispVal -> LST.IOThrowsError a

fromScheme :: forall a. FromScheme a => LST.LispVal -> LST.IOThrowsError a
fromScheme =
  \case
    p@LST.Pointer {} -> LSV.derefPtr p >>= fromScheme_
    v -> fromScheme_ v
{-# INLINABLE fromScheme #-}

-- | Helper, not exported
typeError :: forall a. FromScheme a => LST.LispVal -> LST.IOThrowsError a
typeError v =
  Exc.ExceptT (return (Left (LST.TypeMismatch (schemeTypeName (Proxy @a)) v)))

instance FromScheme LST.LispVal where
  fromScheme_ = return
  {-# INLINE fromScheme_ #-}

instance Typeable a => FromScheme (Opaque a) where
  fromScheme_ =
    \case
      LST.Opaque o | Just o' <- Dyn.fromDynamic o -> return (Opaque o')
      v -> typeError v
  {-# INLINABLE fromScheme_ #-}

instance FromScheme a => FromScheme (Array Int a) where
  fromScheme_ =
    \case
      LST.Vector v -> traverse fromScheme v
      v -> typeError v
  {-# INLINABLE fromScheme_ #-}

instance FromScheme Bool where
  fromScheme_ =
    \case
      LST.Bool b -> return b
      v -> typeError v
  {-# INLINABLE fromScheme_ #-}

instance FromScheme Char where
  fromScheme_ =
    \case
      LST.Char c -> return c
      v -> typeError v
  {-# INLINABLE fromScheme_ #-}

instance FromScheme Double where
  fromScheme_ =
    \case
      LST.Float f -> return f
      v -> typeError v
  {-# INLINABLE fromScheme_ #-}

instance FromScheme Integer where
  fromScheme_ =
    \case
      LST.Number i -> return i
      v -> typeError v
  {-# INLINABLE fromScheme_ #-}

instance {-# OVERLAPPABLE #-} FromScheme a => FromScheme [a] where
  fromScheme_ =
    \case
      LST.List l -> traverse fromScheme l
      v -> typeError v
  {-# INLINABLE fromScheme_ #-}

instance
  ( Ord a
  , FromScheme a
  , FromScheme b
  ) => FromScheme (Map a b) where
  fromScheme_ =
    \case
      LST.HashTable m ->
        Map.fromList <$>
          traverse
            (\(k, v) -> (,) <$> fromScheme k <*> fromScheme v)
            (Map.toList m)
      v -> typeError v
  {-# INLINABLE fromScheme_ #-}

instance FromScheme String where
  fromScheme_ =
    \case
      LST.String s -> return s
      v -> typeError v
  {-# INLINABLE fromScheme_ #-}

instance
  ( FromScheme a
  , FromScheme b
  ) => FromScheme (a, b) where
  fromScheme_ =
    \case
      LST.List [a, b] -> (,) <$> fromScheme a <*> fromScheme b
      v -> typeError v
  {-# INLINABLE fromScheme_ #-}

instance
  ( FromScheme a
  , FromScheme b
  , FromScheme c
  ) => FromScheme (a, b, c) where
  fromScheme_ =
    \case
      LST.List [a, b, c] ->
        (,,)
        <$> fromScheme a
        <*> fromScheme b
        <*> fromScheme c
      v -> typeError v
  {-# INLINABLE fromScheme_ #-}

instance
  ( FromScheme a
  , FromScheme b
  , FromScheme c
  , FromScheme d
  ) => FromScheme (a, b, c, d) where
  fromScheme_ =
    \case
      LST.List [a, b, c, d] ->
        (,,,)
        <$> fromScheme a
        <*> fromScheme b
        <*> fromScheme c
        <*> fromScheme d
      v -> typeError v
  {-# INLINABLE fromScheme_ #-}

instance
  ( FromScheme a
  , FromScheme b
  , FromScheme c
  , FromScheme d
  , FromScheme e
  ) => FromScheme (a, b, c, d, e) where
  fromScheme_ =
    \case
      LST.List [a, b, c, d, e] ->
        (,,,,)
        <$> fromScheme a
        <*> fromScheme b
        <*> fromScheme c
        <*> fromScheme d
        <*> fromScheme e
      v -> typeError v
  {-# INLINABLE fromScheme_ #-}

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

instance
  ( SchemeTypeName a
  , ToSchemeVal a
  , FromScheme b
  ) => FromScheme (a -> LST.IOThrowsError b) where
  fromScheme_ =
    \case
      f | isFunc f ->
        return $
          \a -> do
            -- TODO(lb): the first f is the current continuation, which is
            -- almost certainly incorrect... does it matter?
            lispVal <- LSC.apply f f [toSchemeVal a]
            fromScheme lispVal
      v -> typeError v

instance
  ( SchemeTypeName a
  , ToSchemeVal a
  , SchemeTypeName b
  , ToSchemeVal b
  , FromScheme c
  ) => FromScheme (a -> b -> LST.IOThrowsError c) where
  fromScheme_ =
    \case
      f | isFunc f ->
        return $
          \a b -> do
            -- TODO(lb): the first f is the current continuation, which is
            -- almost certainly incorrect... does it matter?
            lispVal <- LSC.apply f f [toSchemeVal a, toSchemeVal b]
            fromScheme lispVal
      v -> typeError v

instance
  ( SchemeTypeName a
  , ToSchemeVal a
  , SchemeTypeName b
  , ToSchemeVal b
  , SchemeTypeName c
  , ToSchemeVal c
  , FromScheme d
  ) => FromScheme (a -> b -> c -> LST.IOThrowsError d) where
  fromScheme_ =
    \case
      f | isFunc f ->
        return $
          \a b c -> do
            -- TODO(lb): the first f is the current continuation, which is
            -- almost certainly incorrect... does it matter?
            lispVal <- LSC.apply f f [toSchemeVal a, toSchemeVal b, toSchemeVal c]
            fromScheme lispVal
      v -> typeError v
