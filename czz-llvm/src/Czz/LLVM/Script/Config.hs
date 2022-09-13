module Czz.LLVM.Script.Config
  ( extendEnv
  , extendEnv'
  , defaultConfig
  ) where

import qualified Language.Scheme.Types as LST

import           Language.Scheme.CustFunc (CustFunc)
import qualified Language.Scheme.CustFunc as Cust
import           Language.Scheme.Opaque (Opaque(..))  -- for auto

import qualified Czz.LLVM.Config.Type as Conf

extendEnv :: String -> LST.Env -> IO LST.Env
extendEnv pfx e = do
  Cust.extendEnv funcs pfx e
  where
    funcs =
      [ defaultConfig
      ]

extendEnv' :: LST.Env -> IO LST.Env
extendEnv' = extendEnv "czz-llvm"

-- TODO(lb): Expose a lower-level API, i.e., the LLVM AST
defaultConfig :: CustFunc
defaultConfig =
  Cust.CustFunc
  { Cust.custFuncName = "default-config"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto conf)
  }
  where
    conf =
      Conf.LLVMConfig
      { Conf.prog = "in.bc"
      , Conf.entryPoint = "main"
      , Conf.skip = []
      , Conf.onlyNeeded = False
      }
