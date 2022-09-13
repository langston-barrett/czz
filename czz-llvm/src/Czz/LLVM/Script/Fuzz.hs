module Czz.LLVM.Script.Fuzz
  ( extendEnv
  , extendEnv'
  , fuzzer
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

import           Czz.LLVM.Env (Env)
import qualified Czz.LLVM.Fuzz as LFuzz
import           Czz.LLVM.Feedback (Feedback)
import           Czz.LLVM.Overrides (Effect)
import           Czz.LLVM.Translate (Translation)

import qualified Czz.LLVM.Config.Type as Conf

extendEnv ::
  Nonce Nonce.GlobalNonceGenerator LLVM ->
  Nonce Nonce.GlobalNonceGenerator Env ->
  Nonce Nonce.GlobalNonceGenerator Effect ->
  Nonce Nonce.GlobalNonceGenerator Feedback ->
  String ->
  LST.Env ->
  IO LST.Env
extendEnv nllvm nenv neff nfb pfx e = do
  Cust.extendEnv funcs pfx e
  where
    funcs =
      [ fuzzer nllvm nenv neff nfb
      ]

extendEnv' ::
  Nonce Nonce.GlobalNonceGenerator LLVM ->
  Nonce Nonce.GlobalNonceGenerator Env ->
  Nonce Nonce.GlobalNonceGenerator Effect ->
  Nonce Nonce.GlobalNonceGenerator Feedback ->
  LST.Env ->
  IO LST.Env
extendEnv' nllvm nenv neff nfb = extendEnv nllvm nenv neff nfb "czz-llvm"

-- | Helper, not exported
lift3 :: (a -> b -> c -> IO d) -> a -> b -> c -> LST.IOThrowsError d
lift3 f a b c = liftIO (f a b c)
{-# INLINE lift3 #-}

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
