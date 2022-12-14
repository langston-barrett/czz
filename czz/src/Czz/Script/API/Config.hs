module Czz.Script.API.Config
  ( extendEnv
  , defaultFuzzConfig
  )
where

import qualified Language.Scheme.Types as LST

import qualified Language.Scheme.Interop.To.Func.Auto as IAuto
import qualified Language.Scheme.Interop.To.Func as ToFunc
import           Language.Scheme.Interop.CustFunc (CustFunc)
import qualified Language.Scheme.Interop.CustFunc as Cust
import           Language.Scheme.Interop.Opaque (Opaque(..))  -- for auto

import           Czz.Coverage.Bucket.Bucketing (BucketingName(ZeroOneMany))
import qualified Czz.Config.Type as Conf

extendEnv :: String -> LST.Env -> IO LST.Env
extendEnv pfx e = Cust.extendEnv funcs pfx e
  where
    funcs =
      [ defaultFuzzConfig
      ]

defaultFuzzConfig :: CustFunc
defaultFuzzConfig =
  Cust.CustFunc
  { Cust.custFuncName = "default-fuzz-config"
  , Cust.custFuncImpl = ToFunc.toSchemeFunc (IAuto.auto1 conf)
  }
  where
    conf =
      Conf.FuzzConfig
      { Conf.bucketing = ZeroOneMany
      , Conf.gas = Nothing
      , Conf.jobs = 2  -- TODO(lb): ncpu
      , Conf.pathLen = 1
      , Conf.seed = Nothing
      , Conf.stateDir = Just "state/"
      , Conf.tries = Just 10
      }
