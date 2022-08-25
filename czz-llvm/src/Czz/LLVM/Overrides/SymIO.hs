-- TODO(lb): reorganize?

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Czz.LLVM.Overrides.SymIO
  ( overrides
  )
where

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.IORef (IORef)
import qualified Data.IORef as IORef

-- p-u
import           Data.Parameterized.Context ((::>), EmptyCtx)
import qualified Data.Parameterized.Context as Ctx

-- crucible
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.Simulator.RegMap as C
import           Lang.Crucible.Backend (IsSymBackend, IsSymInterface)
import qualified Lang.Crucible.Simulator as C
import           Lang.Crucible.Simulator.OverrideSim (OverrideSim)
import           Lang.Crucible.Simulator.RegMap (RegEntry, RegValue)
import           Lang.Crucible.Types (BVType)

-- crucible-llvm
import           Lang.Crucible.LLVM.Extension (ArchWidth)
import           Lang.Crucible.LLVM.MemModel (HasLLVMAnn, MemOptions)
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import           Lang.Crucible.LLVM.MemModel.Pointer (LLVMPointerType)
import           Lang.Crucible.LLVM.TypeContext (TypeContext)
import           Lang.Crucible.LLVM.Intrinsics (LLVMOverride)
import qualified Lang.Crucible.LLVM.Intrinsics as CLLVM
import qualified Lang.Crucible.LLVM.SymIO as CLLVM

import           Czz.LLVM.QQ (llvmOvr)

-- TODO(lb): newtype for bytestring
-- TODO(lb): docstrings
-- TODO(lb): ioref for file desciptor number, shared with socket API

overrides ::
  IsSymInterface sym =>
  HasLLVMAnn sym =>
  (wptr ~ ArchWidth arch) =>
  CLLVM.HasPtrWidth wptr =>
  (?lc :: TypeContext) =>
  (?memOpts :: CLLVM.MemOptions) =>
  proxy arch ->
  IORef [ByteString] ->
  CLLVM.LLVMFileSystem wptr ->
  [CLLVM.OverrideTemplate p sym arch rtp l a]
overrides proxy pathRef fs =
  [ CLLVM.basic_llvm_override (openDecl proxy pathRef fs)
  , CLLVM.basic_llvm_override (CLLVM.closeFile fs)
  , CLLVM.basic_llvm_override (CLLVM.readFileHandle fs)
  , CLLVM.basic_llvm_override (CLLVM.writeFileHandle fs)
  ]

openDecl ::
  forall proxy p sym arch wptr.
  IsSymInterface sym =>
  HasLLVMAnn sym =>
  (wptr ~ ArchWidth arch) =>
  CLLVM.HasPtrWidth wptr =>
  (?lc :: TypeContext) =>
  (?memOpts :: MemOptions) =>
  proxy arch ->
  IORef [ByteString] ->
  CLLVM.LLVMFileSystem wptr ->
  LLVMOverride
    p
    sym
    (EmptyCtx ::> LLVMPointerType wptr
              ::> BVType 32)
    (BVType 32)
openDecl proxy pathRef fs =
  [llvmOvr| i32 @open( i8*, i32 ) |]
  (\memVar bak args ->
    Ctx.uncurryAssignment (openImpl proxy pathRef bak memVar fs) args)

openImpl ::
  IsSymBackend sym bak =>
  HasLLVMAnn sym =>
  (wptr ~ ArchWidth arch) =>
  CLLVM.HasPtrWidth wptr =>
  (?lc :: TypeContext) =>
  (?memOpts :: MemOptions) =>
  proxy arch ->
  IORef [ByteString] ->
  bak ->
  C.GlobalVar CLLVM.Mem ->
  CLLVM.LLVMFileSystem wptr ->
  RegEntry sym (LLVMPointerType wptr) ->
  RegEntry sym (BVType 32) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (BVType 32))
openImpl _proxy pathRef bak memVar fs pathPtr flags = do
  mem <- C.readGlobal memVar
  fileNm <- liftIO (CLLVM.loadString bak mem (C.regValue pathPtr) Nothing)
  liftIO (IORef.modifyIORef pathRef (BS.pack fileNm:))
  CLLVM.callOpenFile bak memVar fs pathPtr flags
