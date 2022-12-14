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

import qualified Language.Scheme.Interop.To.Func.Auto as IAuto
import qualified Language.Scheme.Interop.To.Func as ToFunc
import           Language.Scheme.Interop.CustFunc (CustFunc)
import qualified Language.Scheme.Interop.CustFunc as Cust
import           Language.Scheme.Interop.Opaque (Opaque(..))  -- for auto

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
  , Cust.custFuncImpl = ToFunc.toSchemeFunc (IAuto.auto1 BS.empty)
  }

singleton :: CustFunc
singleton =
  Cust.CustFunc
  { Cust.custFuncName = "singleton"
  , Cust.custFuncImpl = ToFunc.toSchemeFunc (IAuto.auto BS.singleton)
  }

eq :: CustFunc
eq =
  Cust.CustFunc
  { Cust.custFuncName = "eq"
  , Cust.custFuncImpl = ToFunc.toSchemeFunc (IAuto.auto ((==) @ByteString))
  }
