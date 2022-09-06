{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Czz.LLVM.Overrides.Errno
  ( module Czz.LLVM.Overrides.Errno.Values
  , symbol
  , memType
  , llvmConst
  , global
  , setErrno
  , overrides
  )
where

import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)

import qualified Text.LLVM.AST as L

import           Data.Parameterized.NatRepr (knownNat)

import           Lang.Crucible.Backend (IsSymBackend)
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.Simulator as C
import           Lang.Crucible.Types (TypeRepr(BVRepr))

import qualified Lang.Crucible.LLVM.Bytes as CLLVM
import qualified Lang.Crucible.LLVM.DataLayout as CLLVM
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import           Lang.Crucible.LLVM.MemModel (MemOptions)
import qualified Lang.Crucible.LLVM.MemType as CLLVM
import           Lang.Crucible.LLVM.Intrinsics (OverrideTemplate)
import qualified Lang.Crucible.LLVM.Intrinsics as CLLVM
import qualified Lang.Crucible.LLVM.Translation as CLLVM

import qualified Czz.Log as Log

import           Czz.LLVM.Overrides.Errno.Values
import           Czz.LLVM.Overrides.Util (OverrideConstraints)
import           Czz.LLVM.QQ (llvmOvr, llvmOvrType)

symbol :: L.Symbol
symbol = L.Symbol "errno"

memType :: CLLVM.MemType
memType = CLLVM.i32

llvmConst :: CLLVM.LLVMConst
llvmConst = CLLVM.ZeroConst memType

global :: L.Global
global =
  L.Global
  { L.globalSym = symbol
  , L.globalAttrs = L.emptyGlobalAttrs
  , L.globalType = L.PrimType (L.Integer 32)
  , L.globalValue = Nothing
  , L.globalAlign = Nothing
  , L.globalMetadata = mempty
  }

setErrno ::
  (?memOpts :: MemOptions) =>
  CLLVM.HasLLVMAnn sym =>
  CLLVM.HasPtrWidth wptr =>
  IsSymBackend sym bak =>
  proxy sym ->
  bak ->
  C.GlobalVar CLLVM.Mem ->
  Errno ->
  C.OverrideSim p sym ext rtp args ret ()
setErrno _proxy bak memVar val =
  C.modifyGlobal memVar $ \mem -> liftIO $ do
    let sym = C.backendGetSym bak
    errnoPtr <- CLLVM.doResolveGlobal bak mem symbol
    let cty = BVRepr (knownNat @32)
    let strTy = CLLVM.bitvectorType (CLLVM.bitsToBytes (32 :: Int))
    val' <- errnoToSymBV sym val
    ((),) <$>
      CLLVM.doStore bak mem errnoPtr cty strTy CLLVM.noAlignment val'

--------------------------------------------------------------------------------
-- Overrides

-- TODO(lb): factor out implementation, call in setErrno
errnoLocationDecl ::
  forall proxy p sym arch wptr.
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  [llvmOvrType| i32* @( ) |]
errnoLocationDecl _proxy =
  [llvmOvr| i32* @__errno_location( ) |]
  (\memVar bak _args -> do
    mem <- C.readGlobal memVar
    liftIO (CLLVM.doResolveGlobal bak mem symbol))

overrides ::
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  [OverrideTemplate p sym arch rtp l a]
overrides proxy =
  [ ov (errnoLocationDecl proxy)
  ]
  where ov = CLLVM.basic_llvm_override
