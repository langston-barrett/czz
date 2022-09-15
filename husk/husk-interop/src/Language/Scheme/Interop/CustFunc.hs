module Language.Scheme.Interop.CustFunc
  ( CustFunc(..)
  , extendEnv
  ) where

import qualified Language.Scheme.Types as LST
import qualified Language.Scheme.Variables as LSV

data CustFunc
  = CustFunc
    { custFuncName :: String
    , custFuncImpl :: [LST.LispVal] -> LST.IOThrowsError LST.LispVal
    }

extendEnv :: [CustFunc] -> String -> LST.Env -> IO LST.Env
extendEnv funcs pfx e = LSV.extendEnv e funcs'
  where
    funcs' =
      map (\(CustFunc nm f) -> ((LSV.varNamespace, pfx ++ "-" ++ nm), LST.CustFunc f)) funcs
