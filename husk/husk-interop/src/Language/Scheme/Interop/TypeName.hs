{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Scheme.Interop.TypeName
  ( SchemeTypeName
  , schemeTypeName
  ) where

import           Data.Array (Array)
import qualified Data.List as List
import           Data.Map (Map)
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Typeable as Typ
import           Type.Reflection (Typeable)

import qualified Language.Scheme.Types as LST

import           Language.Scheme.Interop.Opaque (Opaque(..))

class SchemeTypeName a where
  schemeTypeName :: proxy a -> String

instance SchemeTypeName LST.LispVal where
  schemeTypeName _proxy = "lisp value"

instance Typeable a => SchemeTypeName (Opaque a) where
  -- This matches how Husk prints Dynamic values
  schemeTypeName _proxy = "<Haskell " ++ show (Typ.typeRep (Proxy @a)) ++ ">"

instance SchemeTypeName a => SchemeTypeName (Array Int a) where
  -- TODO(lb): how does Husk print Vector/Array?
  schemeTypeName _proxy = unwords ["vector of" , schemeTypeName (Proxy @a)]

instance SchemeTypeName Bool where
  schemeTypeName _proxy = "bool"

instance SchemeTypeName Char where
  schemeTypeName _proxy = "char"

instance SchemeTypeName Double where
  schemeTypeName _proxy = "float"

instance SchemeTypeName Integer where
  schemeTypeName _proxy = "number"

instance {-# OVERLAPPABLE #-} SchemeTypeName a => SchemeTypeName [a] where
  -- This matches how Husk prints list types
  schemeTypeName _proxy = concat ["(" , schemeTypeName (Proxy @a), ")"]

instance (SchemeTypeName a, SchemeTypeName b) => SchemeTypeName (Map a b) where
  -- TODO(lb): how does Husk print hash tables?
  schemeTypeName _proxy =
    unwords
      [ "hash table from"
      , schemeTypeName (Proxy @a)
      , "to"
      , schemeTypeName (Proxy @b)
      ]

instance SchemeTypeName String where
  schemeTypeName _proxy = "string"

-- | Matches how Husk prints lists.
--
-- Helper, not exported
tupleType :: [String] -> String
tupleType ts = concat ["(", unwords ts, ")"]

instance
  ( SchemeTypeName a
  , SchemeTypeName b
  ) => SchemeTypeName (a, b) where
  schemeTypeName _proxy =
    tupleType
      [ schemeTypeName (Proxy @a)
      , schemeTypeName (Proxy @b)
      ]

instance
  ( SchemeTypeName a
  , SchemeTypeName b
  , SchemeTypeName c
  ) => SchemeTypeName (a, b, c) where
  schemeTypeName _proxy =
    tupleType
      [ schemeTypeName (Proxy @a)
      , schemeTypeName (Proxy @b)
      , schemeTypeName (Proxy @c)
      ]

instance
  ( SchemeTypeName a
  , SchemeTypeName b
  , SchemeTypeName c
  , SchemeTypeName d
  ) => SchemeTypeName (a, b, c, d) where
  schemeTypeName _proxy =
    tupleType
      [ schemeTypeName (Proxy @a)
      , schemeTypeName (Proxy @b)
      , schemeTypeName (Proxy @c)
      , schemeTypeName (Proxy @d)
      ]

instance
  ( SchemeTypeName a
  , SchemeTypeName b
  , SchemeTypeName c
  , SchemeTypeName d
  , SchemeTypeName e
  ) => SchemeTypeName (a, b, c, d, e) where
  schemeTypeName _proxy =
    tupleType
      [ schemeTypeName (Proxy @a)
      , schemeTypeName (Proxy @b)
      , schemeTypeName (Proxy @c)
      , schemeTypeName (Proxy @d)
      , schemeTypeName (Proxy @e)
      ]

-- | Helper, not exported
funcType :: [String] -> String
funcType = List.intercalate " -> "

instance
  ( SchemeTypeName a
  , SchemeTypeName b
  ) => SchemeTypeName (a -> LST.IOThrowsError b) where
  schemeTypeName _proxy =
    funcType
      [ schemeTypeName (Proxy @a)
      , schemeTypeName (Proxy @b)
      ]

instance
  ( SchemeTypeName a
  , SchemeTypeName b
  , SchemeTypeName c
  ) => SchemeTypeName (a -> b -> LST.IOThrowsError c) where
  schemeTypeName _proxy =
    funcType
      [ schemeTypeName (Proxy @a)
      , schemeTypeName (Proxy @b)
      , schemeTypeName (Proxy @c)
      ]

instance
  ( SchemeTypeName a
  , SchemeTypeName b
  , SchemeTypeName c
  , SchemeTypeName d
  ) => SchemeTypeName (a -> b -> c -> LST.IOThrowsError d) where
  schemeTypeName _proxy =
    funcType
      [ schemeTypeName (Proxy @a)
      , schemeTypeName (Proxy @b)
      , schemeTypeName (Proxy @c)
      , schemeTypeName (Proxy @d)
      ]
