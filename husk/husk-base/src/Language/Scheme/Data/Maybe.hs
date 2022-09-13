{-# LANGUAGE TypeApplications #-}
module Language.Scheme.Data.Maybe
  ( extendEnv
  , extendEnv'
  , just
  , nothing
  , maybe_
  , catMaybes
  )
where

import           Control.Monad.IO.Class (liftIO)
import           Data.Coerce (coerce)
import qualified Data.Maybe as Maybe

import qualified Language.Scheme.Types as LST

import           Language.Scheme.CustFunc (CustFunc)
import qualified Language.Scheme.CustFunc as Cust
import           Language.Scheme.Opaque (Opaque(..))
import           Language.Scheme.To ()

extendEnv :: String -> LST.Env -> IO LST.Env
extendEnv =
  Cust.extendEnv
    [ just
    , nothing
    , maybe_
    , catMaybes
    ]

extendEnv' :: LST.Env -> IO LST.Env
extendEnv' = extendEnv "maybe"

just :: CustFunc
just =
  Cust.CustFunc
  { Cust.custFuncName = "just"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto impl)
  }
  where
    impl :: LST.LispVal -> Maybe LST.LispVal
    impl = Just

nothing :: CustFunc
nothing =
  Cust.CustFunc
  { Cust.custFuncName = "nothing"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto impl)
  }
  where
    impl :: Maybe LST.LispVal
    impl = Nothing

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
      Maybe LST.LispVal ->
      LST.IOThrowsError LST.LispVal
    maybeM n j x = Maybe.maybe (return n) j x

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
        (coerce (lift1 (return . (Maybe.catMaybes @LST.LispVal))) ::
          [Opaque (Maybe LST.LispVal)] -> LST.IOThrowsError [LST.LispVal])
  }
