{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Czz.LLVM.Init
  ( logToTempFile
  , initState
  )
where

import qualified Control.Lens as Lens
import           Control.Monad (forM_)
import           Data.ByteString (ByteString)
import           Data.Functor.Contravariant ((>$<))
import           Data.IORef (IORef)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.IO as TextIO
import           System.IO (Handle)
import qualified System.IO as IO

import qualified Text.LLVM.AST as L
import qualified Text.LLVM.PP as L

-- p-u
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Map as MapF

-- crucible
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.FunctionHandle as C
import qualified Lang.Crucible.Simulator as C
import           Lang.Crucible.Types (UnitType)

-- crucible-llvm
import qualified Lang.Crucible.LLVM as CLLVM
import           Lang.Crucible.LLVM.Extension (ArchWidth)
import qualified Lang.Crucible.LLVM.Globals as CLLVM
import qualified Lang.Crucible.LLVM.Intrinsics as CLLVM
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import qualified Lang.Crucible.LLVM.Translation as CLLVM
import qualified Lang.Crucible.LLVM.SymIO as CLLVM

-- symio
import qualified Lang.Crucible.SymIO as SymIO

import qualified Czz.Log as Log
import qualified Czz.Log.Concurrent as CLog
import           Czz.Fuzz (CzzPersonality(CzzPersonality))
import           Czz.Overrides (EffectTrace)
import           Czz.Seed (Seed)
import qualified Czz.Seed as Seed

import           Czz.LLVM.Env (Env)
import qualified Czz.LLVM.Env as Env
import qualified Czz.LLVM.Env.Args as Args
import qualified Czz.LLVM.Env.FileSystem as FS
import qualified Czz.LLVM.Overrides as Ov
import           Czz.LLVM.Overrides (Effect)
import qualified Czz.LLVM.Overrides.Skip as Skip
import           Czz.LLVM.Overrides.State.Env as State.Env
import qualified Czz.LLVM.Overrides.SymIO as CzzSymIO
import           Czz.LLVM.Translate (Translation, EntryPoint)
import qualified Czz.LLVM.Translate as Trans

logToTempFile :: Log.Has Text => IO Handle
logToTempFile = do
  -- Forward simulator logs to parent logger
  (_path, logHandle) <- IO.openTempFile "/tmp" "czz.temp"
  let msg = "simulator logging thread exited! "
  let onError = Log.error . (msg <>) . Text.pack . show
  _threadId <- CLog.forkReadHandle onError logHandle (Log.toDebug >$< ?logger)
  return logHandle

-- | Caution: Prints and can exit
initState ::
  Log.Has Text =>
  C.IsSymBackend sym bak =>
  CLLVM.HasLLVMAnn sym =>
  (?memOpts :: CLLVM.MemOptions) =>
  proxy sym ->
  bak ->
  C.HandleAllocator ->
  Translation ->
  -- | Where to put simulator logs
  IO Handle ->
  -- | Trace of read environment variables
  IORef [ByteString] ->
  -- | Trace of opened files
  IORef [ByteString] ->
  IORef (EffectTrace Effect) ->
  Seed t Env Effect ->
  -- | Functions to skip
  [String] ->
  IO (Handle, C.ExecState CzzPersonality sym CLLVM.LLVM (C.RegEntry sym UnitType))
initState _proxy bak halloc translation logHandle envVarRef openedRef effectRef seed skip = do
  Trans.Translation trans memVar entryPoint <- return translation
  let llvmAst = trans Lens.^. CLLVM.modTransModule
  let llvmCtx = trans Lens.^. CLLVM.transContext
  CLLVM.llvmPtrWidth llvmCtx $ \ptrW -> CLLVM.withPtrWidth ptrW $ do
    mem <-
      let ?lc = llvmCtx Lens.^. CLLVM.llvmTypeCtx
      in CLLVM.populateAllGlobals bak (trans Lens.^. CLLVM.globalInitMap)
           =<< CLLVM.initializeAllMemory bak llvmCtx llvmAst

    let globSt = CLLVM.llvmGlobalsToCtx llvmCtx mem
    let sym = C.backendGetSym bak
    let mkPath = SymIO.FileTarget . Text.unpack . Text.decodeUtf8With Text.lenientDecode
    let initFsContent =
          SymIO.emptyInitialFileSystemContents
            { SymIO.concreteFiles =
              Map.mapKeys mkPath (FS.fsMap (Env.fs (Seed.env seed)))
            }
    (initFs, globSt', CLLVM.SomeOverrideSim act) <-
      CLLVM.initialLLVMFileSystem halloc sym ?ptrWidth initFsContent [] globSt

    let intrinsicTypes =
          MapF.union CLLVM.llvmIntrinsicTypes CLLVM.llvmSymIOIntrinsicTypes

    logHand <- logHandle
    let simCtx =
          C.initSimContext
            bak
            intrinsicTypes
            halloc
            logHand
            (C.fnBindingsFromList [])
            (CLLVM.llvmExtensionImpl ?memOpts)
            CzzPersonality
    let st =
          C.InitialState simCtx globSt' C.defaultAbortHandler C.UnitRepr $
            C.runOverrideSim C.UnitRepr $ do
              act  -- see comment on initialLLVMFileSystem
              let args = Env.args (Seed.env seed)
              setupInitState halloc bak entryPoint trans memVar initFs envVarRef openedRef effectRef skip args
    return (logHand, st)

-- | Not exported.
setupInitState ::
  Log.Has Text =>
  C.IsSymBackend sym bak =>
  (wptr ~ ArchWidth arch) =>
  CLLVM.HasPtrWidth wptr =>
  CLLVM.HasLLVMAnn sym =>
  (?memOpts :: CLLVM.MemOptions) =>
  C.HandleAllocator ->
  bak ->
  EntryPoint arch ->
  CLLVM.ModuleTranslation arch ->
  C.GlobalVar CLLVM.Mem ->
  CLLVM.LLVMFileSystem wptr ->
  -- | Trace of read environment variables
  IORef [ByteString] ->
  -- | Trace of opened files
  IORef [ByteString] ->
  IORef (EffectTrace Effect) ->
  -- | Functions to skip
  [String] ->
  Args.Template ->
  C.OverrideSim p sym CLLVM.LLVM rtp args ret ()
setupInitState halloc bak entryPoint trans memVar initFs envVarRef openedRef effectRef skip template = do
  registerDefinedFunctions
  -- TODO(lb): initial file system?
  mainArgs <- Args.genArgs bak trans memVar template
  -- TODO(lb): set up env vars
  registerOverrides

  () <-
    case entryPoint of
      Trans.VoidEntry cfg -> do
        _ <- C.callCFG cfg C.emptyRegMap
        return ()
      Trans.ArgvEntry cfg -> do
        case Ctx.viewAssign (C.regMap mainArgs) of
          Ctx.AssignExtend mainArgs' _envp -> do
            _ <- C.callCFG cfg (C.RegMap mainArgs')
            return ()
      Trans.EnvpEntry cfg -> do
        _ <- C.callCFG cfg mainArgs
        return ()
  return ()

  where
    llvmAst = trans Lens.^. CLLVM.modTransModule
    llvmCtx = trans Lens.^. CLLVM.transContext

    registerDefinedFunctions = do
      forM_ (trans Lens.^. CLLVM.modTransDefs) $ \(decl, _) ->
        CLLVM.registerLazyModuleFn printWarn trans (L.decName decl)

    registerOverrides = do
      envStateVar <- State.Env.mkEnvVar halloc
      let ?lc = llvmCtx Lens.^. CLLVM.llvmTypeCtx
      let decOvs = Ov.overrides trans effectRef envVarRef envStateVar ++
                     CzzSymIO.overrides trans openedRef initFs
      let defOvs = Skip.overrides trans skip
      let ?intrinsicsOpts = CLLVM.defaultIntrinsicsOptions
      CLLVM.register_llvm_overrides llvmAst defOvs decOvs llvmCtx

    -- TODO(lb): stderr
    printWarn (CLLVM.LLVMTranslationWarning s p msg) = do
      let msg' =
            [ Text.pack (show (L.ppSymbol s))
            , Text.pack (show p)
            , msg
            ]
      TextIO.putStrLn $ Text.unwords msg'
