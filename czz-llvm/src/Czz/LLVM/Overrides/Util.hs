{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

module Czz.LLVM.Overrides.Util
  ( OverrideConstraints
  , writeString
  )
where

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)

-- crucible
import           Lang.Crucible.Backend (IsSymBackend, IsSymInterface)
import           Lang.Crucible.Simulator.OverrideSim (OverrideSim)
import qualified Lang.Crucible.Simulator as C
import           Lang.Crucible.Simulator.RegMap (RegValue)

-- crucible-llvm
import qualified Lang.Crucible.LLVM.DataLayout as CLLVM
import           Lang.Crucible.LLVM.Extension (ArchWidth)
import           Lang.Crucible.LLVM.MemModel (HasLLVMAnn, MemOptions)
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import           Lang.Crucible.LLVM.MemModel.Pointer (LLVMPointerType)
import           Lang.Crucible.LLVM.TypeContext (TypeContext)

type OverrideConstraints sym arch wptr =
  ( IsSymInterface sym
  , HasLLVMAnn sym
  , wptr ~ ArchWidth arch
  , CLLVM.HasPtrWidth wptr
  , ?lc :: TypeContext
  , ?memOpts :: MemOptions
  )

writeString ::
  IsSymBackend sym bak =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  bak ->
  C.GlobalVar CLLVM.Mem ->
  ByteString ->
  RegValue sym (LLVMPointerType wptr) ->
  OverrideSim p sym ext rtp args ret ()
writeString _proxy bak memVar bs ptr = do
  C.modifyGlobal memVar $ \mem -> liftIO $ do
    let val = CLLVM.LLVMValString bs
    let ty = CLLVM.llvmValStorableType val
    mem' <- CLLVM.storeRaw bak mem ptr ty CLLVM.noAlignment val
    return ((), mem')
