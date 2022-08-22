{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Czz.JVM.Translate
  ( EntryPoint(..)
  , translate
  )
where

import           Control.Monad (unless)
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Maybe as Maybe
import           Data.Type.Equality (TestEquality(testEquality), (:~:)(Refl))

import qualified Lang.JVM.Codebase as JVM

-- p-u
import qualified Data.Parameterized.Context as Ctx

-- crucible
import qualified Lang.Crucible.FunctionHandle as C

-- c-jvm
import qualified Lang.Crucible.JVM.Context as CJVM
import qualified Lang.Crucible.JVM.Simulate as CJVM
import qualified Lang.Crucible.JVM.Types as CJVM

import qualified Czz.JVM.Config.Type as Conf

-- TODO(lb): support for more entry point types
data EntryPoint where
  SingleArgEntry ::
    C.FnHandle (Ctx.EmptyCtx Ctx.::> CJVM.JVMRefType) ret -> EntryPoint

-- | Allowed to fail/call exit
translate ::
  Conf.Config ->
  IO ( CJVM.JVMContext
     , EntryPoint
     )
translate conf =
  doTranslate
    (Conf.classPath conf)
    (Conf.jars conf)
    (Maybe.fromMaybe "Main" (Conf.entryClass conf))
    (Maybe.fromMaybe "main" (Conf.entryMethod conf))

-- | Not exported.
doTranslate ::
  [String] ->
  [String] ->
  String ->
  String ->
  IO ( CJVM.JVMContext
     , EntryPoint
     )
doTranslate classPath jars entryClass entryMeth = do
  codeBase <-
    JVM.loadCodebase
      jars
      (if null classPath then ["."] else classPath)
      [] -- java bin dirs

  (methClass, meth) <-
    CJVM.findMethod codeBase entryMeth =<<
      CJVM.findClass codeBase entryClass

  unless (JVM.methodIsStatic meth) $ do
    fail $ unlines ["Entry point must be static"]

  halloc <- C.newHandleAllocator
  let extendCtx cs = State.execStateT (mapM (CJVM.extendJVMContext halloc) cs)
  allClasses <- CJVM.findAllRefs codeBase (JVM.className methClass)
  jvmCtx <- extendCtx (methClass:allClasses) =<< CJVM.mkInitialJVMContext halloc

  (CJVM.JVMHandleInfo _ h) <- CJVM.findMethodHandle jvmCtx methClass meth
  let argTys = Ctx.Empty `Ctx.extend` CJVM.refRepr
  case testEquality (C.handleArgTypes h) argTys of
    Nothing -> fail "Entry method must take String[] args"
    Just Refl -> return (jvmCtx, SingleArgEntry h)
