{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Czz.JVM.Init
  ( initState
  )
where

import qualified Control.Monad.Trans.State.Strict as State
import           Data.Functor.Contravariant ((>$<))
import           Data.IORef (IORef)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.IO (Handle)
import qualified System.IO as IO

import qualified Lang.JVM.Codebase as JVM

-- p-u
import qualified Data.Parameterized.Context as Ctx

-- what4
import qualified What4.Interface as What4
import qualified What4.Partial as What4

-- crucible
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.FunctionHandle as C
import qualified Lang.Crucible.Simulator.GlobalState as C
import qualified Lang.Crucible.Simulator as C
import           Lang.Crucible.Types (UnitType)

-- c-jvm
import qualified Lang.Crucible.JVM.Context as CJVM
import qualified Lang.Crucible.JVM.Overrides as CJVM
import qualified Lang.Crucible.JVM.Simulate as CJVM
import qualified Lang.Crucible.JVM.Types as CJVM

import           Czz.Fuzz (CzzPersonality(CzzPersonality))
import qualified Czz.Log.Concurrent as CLog
import qualified Czz.Log as Log
import           Czz.Seed (Seed)
import           Czz.SysTrace (SomeSysTrace)

import           Czz.JVM.Translate (EntryPoint)
import qualified Czz.JVM.Translate as Trans

initState ::
  Log.Has Text =>
  C.IsSymBackend sym bak =>
  bak ->
  CJVM.JVMContext ->
  EntryPoint ->
  C.HandleAllocator ->
  IORef (SomeSysTrace ())  ->
  Seed t () () ->
  IO ( Handle
     , C.ExecState CzzPersonality sym CJVM.JVM (C.RegEntry sym UnitType)
     )
initState bak jvmCtx entryPoint halloc _traceRef _seed = do
  let sym = C.backendGetSym bak
  globals <-
    Map.foldrWithKey
      (initField sym)
      (return globals0)
      (CJVM.staticFields jvmCtx)

  -- Forward simulator logs to parent logger
  (_path, logHandle) <- IO.openTempFile "/tmp" "czz.temp"
  let msg = "[ERROR] simulator logging thread exited! "
  let onError = Log.error . (msg <>) . Text.pack . show
  _threadId <- CLog.forkReadHandle onError logHandle (Log.toDebug >$< ?logger)

  let verbosity = 0
  let simCtx =
        CJVM.jvmSimContext
          bak
          halloc
          logHandle
          jvmCtx
          verbosity
          CzzPersonality
  let st =
        C.InitialState simCtx globals C.defaultAbortHandler C.UnitRepr $
          C.runOverrideSim C.UnitRepr $ do
          _ <-
            State.runStateT
              (mapM_ CJVM.register_jvm_override CJVM.stdOverrides)
              jvmCtx
          -- TODO(lb): ???
          -- _ <- runClassInit halloc ctx classname

          Trans.SingleArgEntry fnHandle <- return entryPoint
          let nullRef = C.RegEntry CJVM.refRepr What4.Unassigned
          -- TODO(lb): args from seed
          let args = C.RegMap (Ctx.Empty `Ctx.extend` nullRef)
          _ret <- C.regValue <$> C.callFnVal (C.HandleFnVal fnHandle) args
          return ()
  return (logHandle, st)
  where
    initField ::
      C.IsSymInterface sym =>
      sym ->
      JVM.FieldId ->
      CJVM.StaticFieldInfo ->
      IO (C.SymGlobalState sym) ->
      IO (C.SymGlobalState sym)
    initField sym fi info m =
      do gs <- m
         z <- CJVM.zeroValue sym (JVM.fieldIdType fi)
         -- For default all static fields to writable
         let writable = What4.truePred sym
         let gs1 = C.insertGlobal (CJVM.staticFieldValue info) z gs
         let gs2 = C.insertGlobal (CJVM.staticFieldWritable info) writable gs1
         pure gs2

    globals0 =
      C.insertGlobal
        (CJVM.dynamicClassTable jvmCtx)
        Map.empty
        C.emptyGlobals
