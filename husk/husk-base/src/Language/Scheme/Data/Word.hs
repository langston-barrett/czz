module Language.Scheme.Data.Word
  ( extendEnv
  , extendEnv'
  , bitReverse8
  )
where

import qualified Data.Word as Word

import qualified Language.Scheme.Types as LST

import qualified Language.Scheme.Interop.To.Func.Auto as IAuto
import           Language.Scheme.Interop.To.Func (Ret(..))  -- for auto
import qualified Language.Scheme.Interop.To.Func as ToFunc
import           Language.Scheme.Interop.CustFunc (CustFunc)
import qualified Language.Scheme.Interop.CustFunc as Cust
import           Language.Scheme.Interop.Opaque (Opaque(..))  -- for auto

extendEnv :: String -> LST.Env -> IO LST.Env
extendEnv =
  Cust.extendEnv
    [ bitReverse8
    ]

extendEnv' :: LST.Env -> IO LST.Env
extendEnv' = extendEnv "word"

bitReverse8 :: CustFunc
bitReverse8 =
  Cust.CustFunc
  { Cust.custFuncName = "bit-reverse-8"
  , Cust.custFuncImpl = ToFunc.toSchemeFunc (IAuto.auto Word.bitReverse8)
  }
