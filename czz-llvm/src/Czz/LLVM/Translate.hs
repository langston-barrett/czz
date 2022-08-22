{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Czz.LLVM.Translate
  ( Translation(..)
  , EntryPoint(..)
  , translate
  )
where

import qualified Control.Lens as Lens
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Exit as Exit

import           Data.LLVM.BitCode (formatError, parseBitCodeFromFile)
import qualified Text.LLVM.AST as L
import qualified Text.LLVM.PP as L

-- p-u
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.Some (Some(Some))

-- crucible
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.FunctionHandle as C

-- crucible-llvm
import qualified Lang.Crucible.LLVM as CLLVM
import           Lang.Crucible.LLVM.Extension (ArchWidth)
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import qualified Lang.Crucible.LLVM.Translation as CLLVM

import qualified Czz.LLVM.Compile as Compile
import qualified Czz.LLVM.Config.Type as Conf

data Translation where
  Translation ::
    CLLVM.ModuleTranslation arch ->
    C.GlobalVar CLLVM.Mem ->
    EntryPoint arch ->
    Translation

-- TODO(lb): support for (i8*, i64)
data EntryPoint arch where
  -- | > int main(void)
  VoidEntry ::
    C.CFG CLLVM.LLVM blocks Ctx.EmptyCtx ret ->
    EntryPoint arch
  -- | > int main(int argc, char *argv[])
  ArgvEntry ::
    ( wptr ~ ArchWidth arch
    , CLLVM.HasPtrWidth wptr
    ) =>
    C.CFG
      CLLVM.LLVM
      blocks
      (C.EmptyCtx
       C.::> CLLVM.LLVMPointerType 32
       C.::> CLLVM.LLVMPointerType wptr)
      ret ->
    EntryPoint arch
  -- | > int main(int argc, char *argv[], char* envp[])
  EnvpEntry ::
    ( wptr ~ ArchWidth arch
    , CLLVM.HasPtrWidth wptr
    ) =>
    C.CFG
      CLLVM.LLVM
      blocks
      (C.EmptyCtx
       C.::> CLLVM.LLVMPointerType 32
       C.::> CLLVM.LLVMPointerType wptr
       C.::> CLLVM.LLVMPointerType wptr)
      ret ->
    EntryPoint arch

-- | Allowed to fail/call exit
translate :: Conf.Config -> IO Translation
translate conf = do
  linkedProg <-
    Compile.linkMusl conf >>=
      \case
        Left err -> do
          putStrLn "Warning: Failed to link:"
          putStrLn (Compile.command err)
          putStrLn "stdout:"
          putStrLn (Compile.sout err)
          putStrLn "stderr:"
          putStrLn (Compile.serr err)
          return (Conf.prog conf)
        Right path -> return path

  llvmAst <-
    parseBitCodeFromFile linkedProg >>=
      \case
        Left err -> quit (formatError err)
        Right ast -> pure ast

  doTranslate
    llvmAst
    (maybe "main" Text.pack (Conf.entryPoint conf))  -- TODO(lb): defaults

  where
    quit err =
      do putStrLn err -- TODO(lb): stderr
         Exit.exitFailure


doTranslate ::
  L.Module ->
  -- | Entry point
  Text ->
  IO Translation
doTranslate llvmAst entryPoint = do
  halloc <- C.newHandleAllocator
  memVar <- liftIO (CLLVM.mkMemVar "czz:llvm_memory" halloc)
  Some (trans :: CLLVM.ModuleTranslation arch) <-
    let ?transOpts = CLLVM.defaultTranslationOptions
    in CLLVM.translateModule halloc memVar llvmAst
  let llvmCtx = trans Lens.^. CLLVM.transContext
  CLLVM.llvmPtrWidth llvmCtx $ \ptrW -> CLLVM.withPtrWidth ptrW $
    Translation trans memVar <$> getEntry trans
  where
    quit err =
      do TextIO.putStrLn err -- TODO(lb): stderr
         Exit.exitFailure

    -- TODO(lb): stderr
    printWarn (CLLVM.LLVMTranslationWarning s p msg) = do
      let msg' =
            [ Text.pack (show (L.ppSymbol s))
            , Text.pack (show p)
            , msg
            ]
      TextIO.putStrLn $ Text.unwords msg'

    getEntry ::
      (wptr ~ ArchWidth arch) =>
      CLLVM.HasPtrWidth wptr =>
      CLLVM.ModuleTranslation arch ->
      IO (EntryPoint arch)
    getEntry trans = do
      C.AnyCFG cfg <-
        liftIO $
          CLLVM.getTranslatedCFG trans (L.Symbol (Text.unpack entryPoint)) >>=
            \case
              Nothing -> quit ("can't find " <> entryPoint)
              Just (_decl, cfg, warns) -> do
                forM_ warns printWarn
                return cfg

      case C.cfgArgTypes cfg of
        Ctx.Empty -> return (VoidEntry cfg)
        (Ctx.Empty Ctx.:> CLLVM.LLVMPointerRepr w Ctx.:> CLLVM.PtrRepr)
          | Just C.Refl <- C.testEquality w (C.knownNat @32)
          -> return (ArgvEntry cfg)
        -- argc, argv, envp
        (Ctx.Empty Ctx.:> CLLVM.LLVMPointerRepr w Ctx.:> CLLVM.PtrRepr Ctx.:> CLLVM.PtrRepr)
          | Just C.Refl <- C.testEquality w (C.knownNat @32)
          -> return (EnvpEntry cfg)
        _ -> quit "Entry point must take no arguments, or arguments like main (i32, i8**) or (i32, i8**, i8**)"
