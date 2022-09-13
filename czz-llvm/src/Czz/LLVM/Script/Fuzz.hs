module Czz.LLVM.Script.Fuzz
  ( extendEnv
  , extendEnv'
  , fuzzer
  ) where

import qualified Control.Lens as Lens
import           Control.Monad.Except (ExceptT(..))  -- for auto
import           Control.Monad.IO.Class (liftIO)
import           Data.Coerce (coerce)
import qualified System.IO as IO

import           Data.Parameterized.Nonce (Nonce, GlobalNonceGenerator)

import           Lang.Crucible.LLVM.Extension (LLVM)
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import qualified Lang.Crucible.LLVM.Translation as CLLVM

import qualified Language.Scheme.Types as LST

import           Language.Scheme.CustFunc (CustFunc)
import qualified Language.Scheme.CustFunc as Cust
import           Language.Scheme.Opaque (Opaque(..))  -- for auto

import qualified Czz.KLimited as KLimit

import           Czz.Script.API.Fuzz (SFuzzer)
import qualified Czz.Script.API.Fuzz as APIFuzz

import qualified Czz.LLVM.Config.Type as Conf
import           Czz.LLVM.Env (Env)
import qualified Czz.LLVM.Fuzz as LFuzz
import           Czz.LLVM.Feedback (Feedback)
import qualified Czz.LLVM.Init as Init
import           Czz.LLVM.Overrides (Effect)
import           Czz.LLVM.Translate (Translation)
import qualified Czz.LLVM.Translate as Trans
import           Czz.LLVM.Script.Overrides (SOverride)
import qualified Czz.LLVM.Script.Overrides as SOverrides

extendEnv ::
  Nonce GlobalNonceGenerator LLVM ->
  Nonce GlobalNonceGenerator Env ->
  Nonce GlobalNonceGenerator Effect ->
  Nonce GlobalNonceGenerator Feedback ->
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
  Nonce GlobalNonceGenerator LLVM ->
  Nonce GlobalNonceGenerator Env ->
  Nonce GlobalNonceGenerator Effect ->
  Nonce GlobalNonceGenerator Feedback ->
  LST.Env ->
  IO LST.Env
extendEnv' nllvm nenv neff nfb =
  extendEnv nllvm nenv neff nfb "czz-llvm"

-- | Helper, not exported
lift4 :: (a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> LST.IOThrowsError e
lift4 f a b c d = liftIO (f a b c d)
{-# INLINE lift4 #-}

fuzzer ::
  Nonce GlobalNonceGenerator LLVM ->
  Nonce GlobalNonceGenerator Env ->
  Nonce GlobalNonceGenerator Effect ->
  Nonce GlobalNonceGenerator Feedback ->
  CustFunc
fuzzer nllvm nenv neff nfb =
  Cust.CustFunc
  { Cust.custFuncName = "fuzzer"
  , Cust.custFuncImpl =
    Cust.evalHuskable
      (coerce (lift4 impl) ::
        Integer ->
        Opaque Conf.LLVMConfig ->
        Opaque Translation ->
        [Opaque SOverride] ->
        LST.IOThrowsError (Opaque SFuzzer))
  }
  where
    impl ::
      Integer ->
      Conf.LLVMConfig ->
      Translation ->
      [SOverride] ->
      IO SFuzzer
    impl k llvmConf t ovs =
      -- TODO(lb): can truncate...
      KLimit.withSomeKLimit (fromIntegral k) $ do
        Trans.Translation trans _memVar _entry <- return t
        let mkHandle = snd <$> IO.openTempFile "/tmp" "czz.temp"
        let llvmCtx = trans Lens.^. CLLVM.transContext
        let extraInit =
              Init.ExtraInit $ \sym nsym ->
                CLLVM.llvmPtrWidth llvmCtx $ \ptrW -> CLLVM.withPtrWidth ptrW $ do
                  SOverrides.registerSOverrides sym nsym trans ovs
        let fz = LFuzz.llvmFuzzer llvmConf t mkHandle extraInit
        return (APIFuzz.SFuzzer nllvm nenv neff nfb fz)
