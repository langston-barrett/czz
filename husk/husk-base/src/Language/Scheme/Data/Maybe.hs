{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Language.Scheme.Data.Maybe
  ( extendEnv
  , extendEnv'
  , just
  , nothing
  , isJust
  , maybe_
  , catMaybes
  , toList
  )
where

import           Control.Monad.Except (ExceptT(..))  -- for auto
import           Control.Monad.IO.Class (liftIO)
import           Data.Coerce (coerce)
import qualified Data.Dynamic as Dyn
import qualified Data.Maybe as Maybe

import qualified Language.Scheme.Types as LST

import           Data.Dyn1 (Dyn1)
import qualified Data.Dyn1 as Dyn1


import qualified Language.Scheme.Interop.To.Func.Auto as IAuto
import           Language.Scheme.Interop.To.Func (Ret(..))  -- for auto
import qualified Language.Scheme.Interop.To.Func as ToFunc
import           Language.Scheme.Interop.CustFunc (CustFunc)
import qualified Language.Scheme.Interop.CustFunc as Cust
import           Language.Scheme.Interop.Opaque (Opaque(..))  -- for auto

extendEnv :: String -> LST.Env -> IO LST.Env
extendEnv =
  Cust.extendEnv
    [ just
    , nothing
    , isJust
    , maybe_
    , catMaybes
    , toList
    ]

extendEnv' :: LST.Env -> IO LST.Env
extendEnv' = extendEnv "maybe"

-- | Helper, not exported
fromDyn1 :: Dyn1 Maybe -> Maybe LST.LispVal
fromDyn1 d =
  case Dyn1.from d of
    Nothing -> Nothing
    Just d' ->
      case Dyn.fromDynamic d' of
        -- It was a Lisp value, unpack it from the dynamic
        j@Just {} -> j
        -- It was a Haskell value, keep it packed in Opaque
        Nothing -> Just (LST.Opaque d')

just :: CustFunc
just =
  Cust.CustFunc
  { Cust.custFuncName = "just"
  , Cust.custFuncImpl = ToFunc.toSchemeFunc (IAuto.auto impl)
  }
  where
    impl :: LST.LispVal -> Dyn1 Maybe
    impl = Dyn1.to . Just

nothing :: CustFunc
nothing =
  Cust.CustFunc
  { Cust.custFuncName = "nothing"
  , Cust.custFuncImpl = ToFunc.toSchemeFunc (IAuto.auto1 impl)
  }
  where
    impl :: Dyn1 Maybe
    impl = Dyn1.to (Nothing :: Maybe ())

isJust :: CustFunc
isJust =
  Cust.CustFunc
  { Cust.custFuncName = "is-just"
  , Cust.custFuncImpl = ToFunc.toSchemeFunc (IAuto.auto impl)
  }
  where
    impl :: Dyn1 Maybe -> Bool
    impl = Dyn1.view Maybe.isJust

maybe_ :: CustFunc
maybe_ =
  Cust.CustFunc
  { Cust.custFuncName = "maybe"
  , Cust.custFuncImpl = ToFunc.toSchemeFunc (IAuto.auto maybeM)
  }
  where
    maybeM ::
      LST.LispVal ->
      (LST.LispVal -> LST.IOThrowsError LST.LispVal) ->
      Dyn1 Maybe ->
      LST.IOThrowsError LST.LispVal
    maybeM n j = Maybe.maybe (return n) j . fromDyn1

-- | Helper, not exported
lift1 :: (a -> IO b) -> a -> LST.IOThrowsError b
lift1 f a = liftIO (f a)
{-# INLINE lift1 #-}

catMaybes :: CustFunc
catMaybes =
  Cust.CustFunc
  { Cust.custFuncName = "cat"
  , Cust.custFuncImpl =
      ToFunc.toSchemeFunc
        (coerce (lift1 (return . Maybe.catMaybes @LST.LispVal . map fromDyn1)) ::
          [Opaque (Dyn1 Maybe)] -> LST.IOThrowsError [LST.LispVal])
  }

toList :: CustFunc
toList =
  Cust.CustFunc
  { Cust.custFuncName = "to-list"
  , Cust.custFuncImpl =
      ToFunc.toSchemeFunc
        (coerce (lift1 (return . Maybe.maybeToList @LST.LispVal . fromDyn1)) ::
          Opaque (Dyn1 Maybe) -> LST.IOThrowsError [Opaque LST.LispVal])
  }
