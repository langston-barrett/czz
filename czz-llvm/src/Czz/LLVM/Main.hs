{-# LANGUAGE RankNTypes #-}

module Czz.LLVM.Main (main) where

import qualified System.Exit as Exit

-- crucible-llvm
import           Lang.Crucible.LLVM.Extension (LLVM)

import qualified Czz.Config.Type as CConf
import           Czz.Fuzz (Fuzzer)
import           Czz.KLimited (IsKLimited)
import qualified Czz.KLimited as KLimit
import qualified Czz.Fuzz as Fuzz
import qualified Czz.Log as Log
import qualified Czz.Script as Script
import qualified Czz.Stop as Stop

import qualified Czz.LLVM.Config.CLI as CLI
import           Czz.LLVM.Config.Type (LLVMConfig)
import qualified Czz.LLVM.Config.Type as Conf
import           Czz.LLVM.Env (Env)
import qualified Czz.LLVM.Fuzz as LFuzz
import           Czz.LLVM.Feedback (Feedback)
import qualified Czz.LLVM.Init as Init
import           Czz.LLVM.Overrides (Effect)
import qualified Czz.LLVM.Script as LScript
import qualified Czz.LLVM.Translate as Trans

-- | CLI entry point
main :: IO Exit.ExitCode
main = do
  conf <- CLI.cliConfig
  let commonConf = Conf.common conf
  let baseConf = CConf.base commonConf
  let llvmConf = Conf.llvm conf
  case CConf.command commonConf of
    CConf.CmdFuzz fuzzConf -> do
      withFuzzer llvmConf (CConf.pathLen fuzzConf) $ \fuzzer -> do
        doFuzz baseConf fuzzConf fuzzer
    CConf.CmdScript scriptConf ->
      Script.run baseConf scriptConf (LScript.extendEnv "czz-llvm")
  return Exit.ExitSuccess

  where
    withFuzzer ::
      LLVMConfig ->
      Int ->
      (forall k. IsKLimited k => Fuzzer LLVM Env Effect k Feedback -> IO a) ->
      IO a
    withFuzzer llvmConf kLimit k =
      KLimit.withSomeKLimit kLimit $ do
        translation <- Trans.translate llvmConf  -- Allowed to fail/call exit
        -- TODO(lb): non-void logger
        let simLog = Log.with Log.void Init.logToTempFile
        k (LFuzz.llvmFuzzer llvmConf translation simLog)

    doFuzz baseConf fuzzConf fuzzer = do
      stop <- Stop.new
      _finalState <- Fuzz.main baseConf fuzzConf stop fuzzer
      return ()
