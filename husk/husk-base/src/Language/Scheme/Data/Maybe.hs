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
  )
where

import           Control.Monad.IO.Class (liftIO)
import           Data.Coerce (coerce)
import qualified Data.Dynamic as Dyn
import qualified Data.Maybe as Maybe

import qualified Language.Scheme.Types as LST

import           Language.Scheme.CustFunc (CustFunc)
import qualified Language.Scheme.CustFunc as Cust
import           Language.Scheme.Dyn1 (Dyn1)
import qualified Language.Scheme.Dyn1 as Dyn1
import           Language.Scheme.Opaque (Opaque(..))
import           Language.Scheme.To ()

extendEnv :: String -> LST.Env -> IO LST.Env
extendEnv =
  Cust.extendEnv
    [ just
    , nothing
    , isJust
    , maybe_
    , catMaybes
    ]

extendEnv' :: LST.Env -> IO LST.Env
extendEnv' = extendEnv "maybe"

-- | Helper, not exported
fromDyn1 :: Dyn1 Maybe -> Maybe LST.LispVal
fromDyn1 d =
  case Dyn1.fromDyn1 d of
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
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto impl)
  }
  where
    impl :: LST.LispVal -> Dyn1 Maybe
    impl = Dyn1.toDyn1 . Just

nothing :: CustFunc
nothing =
  Cust.CustFunc
  { Cust.custFuncName = "nothing"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto impl)
  }
  where
    impl :: Dyn1 Maybe
    impl = Dyn1.toDyn1 (Nothing :: Maybe ())

isJust :: CustFunc
isJust =
  Cust.CustFunc
  { Cust.custFuncName = "is-just"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto impl)
  }
  where
    impl :: Dyn1 Maybe -> Bool
    impl = Dyn1.viewDyn1 Maybe.isJust

maybe_ :: CustFunc
maybe_ =
  Cust.CustFunc
  { Cust.custFuncName = "maybe"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto maybeM)
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
      Cust.evalHuskable
        (coerce (lift1 (return . (Maybe.catMaybes @LST.LispVal . map fromDyn1))) ::
          [Opaque (Dyn1 Maybe)] -> LST.IOThrowsError [LST.LispVal])
  }
