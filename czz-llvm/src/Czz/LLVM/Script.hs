module Czz.LLVM.Script
  ( extendEnv
  ) where

import           Control.Monad (foldM)

import qualified Data.Parameterized.Nonce as Nonce

import qualified Language.Scheme.Types as LST

import qualified Czz.LLVM.Script.Config as SConfig
import qualified Czz.LLVM.Script.Fuzz as SFuzz
import qualified Czz.LLVM.Script.Overrides as SOverrides
import qualified Czz.LLVM.Script.Translate as STranslate

extendEnv :: String -> LST.Env -> IO LST.Env
extendEnv pfx e = do
  nllvm <- Nonce.freshNonce Nonce.globalNonceGenerator
  nenv <- Nonce.freshNonce Nonce.globalNonceGenerator
  neff <- Nonce.freshNonce Nonce.globalNonceGenerator
  nfb <- Nonce.freshNonce Nonce.globalNonceGenerator
  let libs =
        [ SConfig.extendEnv pfx
        , SFuzz.extendEnv nllvm nenv neff nfb pfx
        , SOverrides.extendEnv pfx
        , STranslate.extendEnv pfx
        ]
  foldM (\env lib -> lib env) e libs

