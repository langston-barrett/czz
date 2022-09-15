module Czz.LLVM.Script.Config
  ( extendEnv
  , extendEnv'
  , defaultConfig
  ) where

import qualified Language.Scheme.Types as LST

import qualified Language.Scheme.Interop.To.Func.Auto as IAuto
import qualified Language.Scheme.Interop.To.Func as ToFunc
import           Language.Scheme.Interop.CustFunc (CustFunc)
import qualified Language.Scheme.Interop.CustFunc as Cust
import           Language.Scheme.Interop.Opaque (Opaque(..))  -- for auto

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
  , Cust.custFuncImpl = ToFunc.toSchemeFunc (IAuto.auto1 conf)
  }
  where
    conf =
      Conf.LLVMConfig
      { Conf.prog = "in.bc"
      , Conf.entryPoint = "main"
      , Conf.skip = []
      , Conf.onlyNeeded = False
      }
