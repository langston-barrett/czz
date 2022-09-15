{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Scheme.Interop.To.Func
  ( ToSchemeFunc
  , toSchemeFunc
  , Ret(..)
  ) where

import qualified Control.Monad.Except as Exc

import qualified Language.Scheme.Types as LST

import           Language.Scheme.Interop.To.Val (ToSchemeVal, toSchemeVal)
import           Language.Scheme.Interop.From (FromScheme, fromScheme)

-- | Values that are marked as 'Ret' in function signatures are the overall
-- return type of the Haskell function being exposed to Scheme.
--
-- Intended to be used in conjunction with 'Data.Coerce.coerce'.
newtype Ret a = Ret { getRet :: a }
  deriving Show

class ToSchemeFunc a where
  toSchemeFunc :: a -> [LST.LispVal] -> LST.IOThrowsError LST.LispVal

  -- | Create a zero-arity function via 'Ret'
  default toSchemeFunc :: ToSchemeVal a => a -> [LST.LispVal] -> LST.IOThrowsError LST.LispVal
  toSchemeFunc a = toSchemeFunc (Ret a)

--------------------------------------------------------------------------------
-- Pure functions

-- | Create a zero-arity function
instance ToSchemeVal a => ToSchemeFunc (Ret a) where
  toSchemeFunc x as =
    case as of
      [] -> return (toSchemeVal (getRet x))
      _ -> Exc.throwError (LST.NumArgs (Just 0) as)

instance (FromScheme a, ToSchemeVal b) => ToSchemeFunc (a -> Ret b) where
  toSchemeFunc f as =
    case as of
      [a] -> toSchemeVal . getRet <$> (f <$> fromScheme a)
      _ -> Exc.throwError (LST.NumArgs (Just 1) as)

instance
  ( FromScheme a
  , FromScheme b
  , ToSchemeVal c
  ) => ToSchemeFunc (a -> b -> Ret c) where
  toSchemeFunc f as =
    case as of
      [a, b] ->
        toSchemeVal . getRet <$>
          (f
           <$> fromScheme a
           <*> fromScheme b)
      _ -> Exc.throwError (LST.NumArgs (Just 2) as)

instance
  ( FromScheme a
  , FromScheme b
  , FromScheme c
  , ToSchemeVal d
  ) => ToSchemeFunc (a -> b -> c -> Ret d) where
  toSchemeFunc f as =
    case as of
      [a, b, c] ->
        toSchemeVal . getRet <$>
          (f
           <$> fromScheme a
           <*> fromScheme b
           <*> fromScheme c)
      _ -> Exc.throwError (LST.NumArgs (Just 2) as)

--------------------------------------------------------------------------------
-- Effectful functions

-- | Create a zero-arity function
instance ToSchemeVal a => ToSchemeFunc (LST.IOThrowsError a) where
  toSchemeFunc x as =
    case as of
      [] -> toSchemeVal <$> x
      _ -> Exc.throwError (LST.NumArgs (Just 0) as)

instance
  ( FromScheme a
  , ToSchemeVal b
  ) => ToSchemeFunc (a -> LST.IOThrowsError b) where
  toSchemeFunc f as =
    case as of
      [a] -> fmap toSchemeVal . f =<< fromScheme a
      _ -> Exc.throwError (LST.NumArgs (Just 1) as)

instance
  ( FromScheme a
  , FromScheme b
  , ToSchemeVal c
  ) => ToSchemeFunc (a -> b -> LST.IOThrowsError c) where
  toSchemeFunc f as =
    case as of
      [a, b] -> do
        a' <- fromScheme a
        b' <- fromScheme b
        toSchemeVal <$> f a' b'
      _ -> Exc.throwError (LST.NumArgs (Just 2) as)

instance
  ( FromScheme a
  , FromScheme b
  , FromScheme c
  , ToSchemeVal d
  ) => ToSchemeFunc (a -> b -> c -> LST.IOThrowsError d) where
  toSchemeFunc f as =
    case as of
      [a, b, c] -> do
        a' <- fromScheme a
        b' <- fromScheme b
        c' <- fromScheme c
        toSchemeVal <$> f a' b' c'
      _ -> Exc.throwError (LST.NumArgs (Just 3) as)

instance
  ( FromScheme a
  , FromScheme b
  , FromScheme c
  , FromScheme d
  , ToSchemeVal e
  ) => ToSchemeFunc (a -> b -> c -> d -> LST.IOThrowsError e) where
  toSchemeFunc f as =
    case as of
      [a, b, c, d] -> do
        a' <- fromScheme a
        b' <- fromScheme b
        c' <- fromScheme c
        d' <- fromScheme d
        toSchemeVal <$> f a' b' c' d'
      _ -> Exc.throwError (LST.NumArgs (Just 4) as)
