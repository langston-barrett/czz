module Language.Scheme.Data.Word
  ( extendEnv
  , extendEnv'
  , bitReverse8
  )
where

import qualified Data.Word as Word

import qualified Language.Scheme.Types as LST

import           Language.Scheme.CustFunc (CustFunc)
import qualified Language.Scheme.CustFunc as Cust
import           Language.Scheme.To ()

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
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.opaque1 Word.bitReverse8)
  }
