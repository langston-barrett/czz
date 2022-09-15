module Czz.LLVM.Script.Translate
  ( extendEnv
  , extendEnv'
  , translate
  ) where

import           Control.Monad.Except (ExceptT(..))  -- for auto
import           Control.Monad.IO.Class (liftIO)

import qualified Language.Scheme.Types as LST

import qualified Language.Scheme.Interop.To.Func.Auto as IAuto
import qualified Language.Scheme.Interop.To.Func as ToFunc
import           Language.Scheme.Interop.CustFunc (CustFunc)
import qualified Language.Scheme.Interop.CustFunc as Cust
import           Language.Scheme.Interop.Opaque (Opaque(..))  -- for auto

import qualified Czz.LLVM.Translate as Trans

extendEnv :: String -> LST.Env -> IO LST.Env
extendEnv pfx e = do
  Cust.extendEnv funcs pfx e
  where
    funcs =
      [ translate
      ]

extendEnv' :: LST.Env -> IO LST.Env
extendEnv' = extendEnv "czz-llvm"

-- | Helper, not exported
lift1 :: (a -> IO b) -> a -> LST.IOThrowsError b
lift1 f a = liftIO (f a)
{-# INLINE lift1 #-}

-- TODO(lb): Expose a lower-level API, i.e., the LLVM AST
translate :: CustFunc
translate =
  Cust.CustFunc
  { Cust.custFuncName = "translate"
  , Cust.custFuncImpl = ToFunc.toSchemeFunc (IAuto.auto (lift1 Trans.translate))
  }
