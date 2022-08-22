{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Czz.LLVM.Overrides.Libc
  ( Effect(..)
  , FprintfEffect(..)
  , StrcpyEffect(..)
  , overrides
  )
where

import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (liftIO)
import qualified Data.BitVector.Sized as BV
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.IORef (IORef)

import qualified Text.LLVM.AST as L

import           Data.Parameterized.Context ((::>), EmptyCtx)
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.NatRepr (knownNat)

import qualified What4.Interface as What4

-- crucible
import           Lang.Crucible.Backend (IsSymBackend, IsSymInterface, backendGetSym)
import qualified Lang.Crucible.Backend as C
import           Lang.Crucible.Simulator.OverrideSim (OverrideSim)
import           Lang.Crucible.Simulator.RegMap (RegEntry, RegValue)
import qualified Lang.Crucible.Simulator as C
import           Lang.Crucible.Types (BVType)

-- crucible-llvm
import qualified Lang.Crucible.LLVM.Bytes as CLLVM
import qualified Lang.Crucible.LLVM.DataLayout as CLLVM
import           Lang.Crucible.LLVM.Extension (ArchWidth)
import           Lang.Crucible.LLVM.QQ (llvmOvr)
import           Lang.Crucible.LLVM.MemModel (HasLLVMAnn, MemOptions)
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import           Lang.Crucible.LLVM.MemModel.Pointer (LLVMPointerType)
import           Lang.Crucible.LLVM.Intrinsics (OverrideTemplate, LLVMOverride)
import qualified Lang.Crucible.LLVM.Intrinsics as CLLVM
import           Lang.Crucible.LLVM.Translation (VarArgs)
import           Lang.Crucible.LLVM.TypeContext (TypeContext)

import qualified Czz.Log as Log
import           Czz.SysTrace (SomeSysTrace)

import qualified Czz.LLVM.Overrides.State.Env as State.Env
import           Czz.LLVM.Overrides.Type (Override(..), toLLVMOverride)
import           Czz.LLVM.Overrides.Util (OverrideConstraints)
import qualified Czz.LLVM.Unimplemented as Unimpl

data Effect
  = Fprintf !FprintfEffect
  | Strcpy !StrcpyEffect
  deriving (Eq, Ord, Show)

_Fprintf :: Lens.Prism' Effect FprintfEffect
_Fprintf =
  Lens.prism'
    Fprintf
    (\case
      Fprintf eff -> Just eff
      _ -> Nothing)

_Strcpy :: Lens.Prism' Effect StrcpyEffect
_Strcpy =
  Lens.prism'
    Strcpy
    (\case
      Strcpy eff -> Just eff
      _ -> Nothing)

overrides ::
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (SomeSysTrace eff) ->
  Lens.Prism' eff Effect ->
  IORef [ByteString] ->
  C.GlobalVar State.Env.EnvState ->
  [OverrideTemplate p sym arch rtp l a]
overrides proxy effects inj _envTrace envVar =
  [ ov (fprintfDecl proxy envVar effects (inj . _Fprintf))
  , ov (strcpyDecl proxy envVar effects (inj . _Strcpy))
  ]
  where ov = CLLVM.basic_llvm_override

------------------------------------------------------------------------
-- ** fprintf

data FprintfEffect
  = FprintfSuccess
  deriving (Eq, Ord, Show)

-- | Unsound!
--
-- TODO(lb): also generate error conditions
fprintfDecl ::
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  C.GlobalVar State.Env.EnvState ->
  IORef (SomeSysTrace eff) ->
  Lens.Prism' eff FprintfEffect ->
  LLVMOverride
    p
    sym
    (EmptyCtx
     ::> LLVMPointerType wptr
     ::> LLVMPointerType wptr
     ::> VarArgs)
    (BVType 32)
fprintfDecl proxy envVar effects inj =
  [llvmOvr| i32 @fprintf( %struct._IO_FILE*, i8*, ... ) |] $
  \memVar bak args ->
    let ov = fprintfOverride proxy
    in toLLVMOverride bak effects inj ov memVar envVar args

fprintfOverride ::
  Log.Has String =>
  (wptr ~ ArchWidth arch) =>
  CLLVM.HasPtrWidth wptr =>
  proxy arch ->
  Override
    arch
    FprintfEffect
    (EmptyCtx
     ::> LLVMPointerType wptr
     ::> LLVMPointerType wptr
     ::> VarArgs)
    (BVType 32)
fprintfOverride proxy =
  Override
  { genEffect = \_bak _memVar _envVar _args -> return FprintfSuccess
  , doEffect =
      \bak e memVar _envVar args ->
        Ctx.uncurryAssignment (fprintfImpl proxy bak e memVar) args
  }

fprintfImpl ::
  IsSymBackend sym bak =>
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  bak ->
  FprintfEffect ->
  C.GlobalVar CLLVM.Mem ->
  RegEntry sym (LLVMPointerType wptr) ->
  RegEntry sym (LLVMPointerType wptr) ->
  RegEntry sym VarArgs ->
  OverrideSim p sym ext rtp args ret (RegValue sym (BVType 32))
fprintfImpl _proxy bak e memVar stream format _varArgs = do
  FprintfSuccess <- return e  -- check for missing pattern matches
  let sym = backendGetSym bak
  mem <- C.readGlobal memVar
  outPtr <- liftIO (CLLVM.doResolveGlobal bak mem (L.Symbol "stdout"))
  errPtr <- liftIO (CLLVM.doResolveGlobal bak mem (L.Symbol "stderr"))
  let ty = CLLVM.bitvectorType (CLLVM.bitsToBytes (What4.natValue ?ptrWidth))
  let cty = CLLVM.LLVMPointerRepr ?ptrWidth
  stdout <- liftIO (CLLVM.doLoad bak mem outPtr ty cty CLLVM.noAlignment)
  stderr <- liftIO (CLLVM.doLoad bak mem errPtr ty cty CLLVM.noAlignment)
  streamIsStdout <- liftIO (CLLVM.ptrEq sym ?ptrWidth stdout (C.regValue stream))
  streamIsStderr <- liftIO (CLLVM.ptrEq sym ?ptrWidth stderr (C.regValue stream))
  notStdoutOrStderr <-
    liftIO (What4.notPred sym =<< What4.orPred sym streamIsStdout streamIsStderr)
  C.symbolicBranches
    C.emptyRegMap
    [ ( notStdoutOrStderr
      , Unimpl.throw Unimpl.FprintfFile
      , Nothing
      )
    , ( What4.truePred sym
      , do let fmt = C.regValue format
           output <- liftIO (BS.pack <$> CLLVM.loadString bak mem fmt Nothing)
           liftIO (Log.log ?logger ("Program called `fprintf`: " <> show output))
           liftIO (What4.bvLit sym (knownNat @32) (BV.mkBV (knownNat @32) 0))
      , Nothing
      )
    ]

------------------------------------------------------------------------
-- ** strcpy

data StrcpyEffect = StrcpyEffect
  deriving (Eq, Ord, Show)

strcpyDecl ::
  IsSymInterface sym =>
  HasLLVMAnn sym =>
  (wptr ~ ArchWidth arch) =>
  CLLVM.HasPtrWidth wptr =>
  (?lc :: TypeContext) =>
  (?memOpts :: MemOptions) =>
  proxy arch ->
  C.GlobalVar State.Env.EnvState ->
  IORef (SomeSysTrace eff) ->
  Lens.Prism' eff StrcpyEffect ->
  LLVMOverride
    p
    sym
    (EmptyCtx
     ::> LLVMPointerType wptr
     ::> LLVMPointerType wptr)
    (LLVMPointerType wptr)
strcpyDecl proxy envVar effects inj =
  [llvmOvr| i8* @strcpy( i8*, i8* ) |] $
  \memVar bak args ->
    let ov = strcpyOverride proxy
    in toLLVMOverride bak effects inj ov memVar envVar args

strcpyOverride ::
  (wptr ~ ArchWidth arch) =>
  CLLVM.HasPtrWidth wptr =>
  proxy arch ->
  Override
    arch
    StrcpyEffect
    (EmptyCtx
     ::> LLVMPointerType wptr
     ::> LLVMPointerType wptr)
    (LLVMPointerType wptr)
strcpyOverride proxy =
  Override
  { genEffect = \_bak _memVar _envVar _args -> return StrcpyEffect
  , doEffect =
      \bak e memVar _envVar args ->
        Ctx.uncurryAssignment (strcpyImpl proxy bak e memVar) args
  }

-- | Unsound!
strcpyImpl ::
  IsSymBackend sym bak =>
  HasLLVMAnn sym =>
  (wptr ~ ArchWidth arch) =>
  CLLVM.HasPtrWidth wptr =>
  (?lc :: TypeContext) =>
  (?memOpts :: MemOptions) =>
  proxy arch ->
  bak ->
  StrcpyEffect ->
  C.GlobalVar CLLVM.Mem ->
  RegEntry sym (LLVMPointerType wptr) ->
  RegEntry sym (LLVMPointerType wptr) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (LLVMPointerType wptr))
strcpyImpl _proxy bak e memVar (C.regValue -> dest) (C.regValue -> src) = do
  StrcpyEffect <- return e  -- check for missing pattern matches
  C.modifyGlobal memVar $ \mem -> liftIO $ do
    let sym = C.backendGetSym bak
    strLen <- CLLVM.strLen bak mem src
    -- Add 1 for null terminator
    memLen <-
      What4.bvAdd sym strLen =<< What4.bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth 1)
    mem' <- CLLVM.doMemcpy bak ?ptrWidth mem True dest src memLen
    return (dest, mem')
