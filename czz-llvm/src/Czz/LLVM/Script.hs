module Czz.LLVM.Script
  ( extendEnv
  ) where

import           Control.Monad.Except (ExceptT(..))  -- for auto
import           Control.Monad.IO.Class (liftIO)
import qualified System.IO as IO

import           Data.Parameterized.Nonce (Nonce)
import qualified Data.Parameterized.Nonce as Nonce

import           Lang.Crucible.LLVM.Extension (LLVM)

import qualified Language.Scheme.Types as LST

import           Language.Scheme.CustFunc (CustFunc)
import qualified Language.Scheme.CustFunc as Cust
import           Language.Scheme.Opaque (Opaque(..))  -- for auto

import qualified Czz.KLimited as KLimit

import           Czz.Script.API.Fuzz (SFuzzer)
import qualified Czz.Script.API.Fuzz as APIFuzz

import qualified Czz.LLVM.Fuzz as LFuzz
import qualified Czz.LLVM.Config.Type as Conf
import           Czz.LLVM.Env (Env)
import           Czz.LLVM.Feedback (Feedback)
import           Czz.LLVM.Overrides (Effect)
import           Czz.LLVM.Translate (Translation)
import qualified Czz.LLVM.Translate as Trans

extendEnv ::
  String ->
  LST.Env ->
  IO LST.Env
extendEnv pfx e = do
  nllvm <- Nonce.freshNonce Nonce.globalNonceGenerator
  nenv <- Nonce.freshNonce Nonce.globalNonceGenerator
  neff <- Nonce.freshNonce Nonce.globalNonceGenerator
  nfb <- Nonce.freshNonce Nonce.globalNonceGenerator
  Cust.extendEnv (funcs nllvm nenv neff nfb) pfx e
  where
    funcs nllvm nenv neff nfb =
      [ defaultConfig
      , translate
      , fuzzer nllvm nenv neff nfb
      ]

-- TODO(lb): these could probably be typeclass-programmed in CustFunc..?

-- | Helper, not exported
_lift :: IO a -> LST.IOThrowsError a
_lift = liftIO
{-# INLINE _lift #-}

-- | Helper, not exported
lift1 :: (a -> IO b) -> a -> LST.IOThrowsError b
lift1 f a = liftIO (f a)
{-# INLINE lift1 #-}

-- | Helper, not exported
_lift2 :: (a -> b -> IO c) -> a -> b -> LST.IOThrowsError c
_lift2 f a b = liftIO (f a b)
{-# INLINE _lift2 #-}

-- | Helper, not exported
lift3 :: (a -> b -> c -> IO d) -> a -> b -> c -> LST.IOThrowsError d
lift3 f a b c = liftIO (f a b c)
{-# INLINE lift3 #-}

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

-- TODO(lb): Expose a lower-level API, i.e., the LLVM AST
translate :: CustFunc
translate =
  Cust.CustFunc
  { Cust.custFuncName = "translate"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto (lift1 Trans.translate))
  }

fuzzer ::
  Nonce Nonce.GlobalNonceGenerator LLVM ->
  Nonce Nonce.GlobalNonceGenerator Env ->
  Nonce Nonce.GlobalNonceGenerator Effect ->
  Nonce Nonce.GlobalNonceGenerator Feedback ->
  CustFunc
fuzzer nllvm nenv neff nfb =
  Cust.CustFunc
  { Cust.custFuncName = "fuzzer"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto (lift3 impl))
  }
  where
    impl ::
      Integer ->
      Conf.LLVMConfig ->
      Translation ->
      IO SFuzzer
    impl k llvmConf trans =
      -- TODO(lb): can truncate...
      KLimit.withSomeKLimit (fromIntegral k) $ do
        let mkHandle = snd <$> IO.openTempFile "/tmp" "czz.temp"
        let fz = LFuzz.llvmFuzzer llvmConf trans mkHandle
        return (APIFuzz.SFuzzer nllvm nenv neff nfb fz)
