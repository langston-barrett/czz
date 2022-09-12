{-# LANGUAGE TypeApplications #-}

module Language.Scheme.ByteString
  ( extendEnv
  , empty
  , singleton
  , eq
  )
where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Language.Scheme.Types as LST

import           Language.Scheme.CustFunc (CustFunc)
import qualified Language.Scheme.CustFunc as Cust
import           Language.Scheme.Opaque (Opaque(..))  -- for auto

extendEnv :: String -> LST.Env -> IO LST.Env
extendEnv =
  Cust.extendEnv
    [ empty
    , singleton
    , eq
    ]

empty :: CustFunc
empty =
  Cust.CustFunc
  { Cust.custFuncName = "empty"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto BS.empty)
  }

singleton :: CustFunc
singleton =
  Cust.CustFunc
  { Cust.custFuncName = "singleton"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto BS.singleton)
  }

eq :: CustFunc
eq =
  Cust.CustFunc
  { Cust.custFuncName = "eq"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto ((==) @ByteString))
  }
