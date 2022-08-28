{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Czz.LLVM.Env.Args
  ( Template
  , empty
  , envp
  , argvLength
  , envpLength
  , addArg
  , rmArg
  , mutArg
  , mutArgs
  , mutArgsA
  , addEnv
  , addWellFormedEnv
  , rmEnv
  , mutEnv
  , mutEnvs
  , mutEnvsA
  , genArgs
  )
where

import           Prelude hiding (null, length)

import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (foldM)
import qualified Data.BitVector.Sized as BV
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Functor ((<&>))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- p-u
import qualified Data.Parameterized.Context as Ctx
import           Data.Parameterized.NatRepr (knownNat, natValue)

-- what4
import qualified What4.Interface as What4

-- crucible
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.Simulator as C

-- crucible-llvm
import qualified Lang.Crucible.LLVM.Bytes as CLLVM
import qualified Lang.Crucible.LLVM.DataLayout as CLLVM
import           Lang.Crucible.LLVM.Extension (ArchWidth)
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import qualified Lang.Crucible.LLVM.MemType as CLLVM
import qualified Lang.Crucible.LLVM.Translation as CLLVM
import qualified Lang.Crucible.LLVM.TypeContext as CLLVM

import           Czz.LLVM.CString (CString)
import qualified Czz.LLVM.CString as CString

data Template
  = Template
      { argv :: Seq CString
      -- | These CONVENTIONALLY have an @=@.
      , envp :: Seq CString
      }
  deriving (Eq, Ord, Show)

empty :: Template
empty =
  Template
    -- Start with one arg (argv[0], i.e. name of the program) to more closely
    -- match normal behavior
    { argv = Seq.singleton (CString.terminate "fake")
    , envp = Seq.empty
    }

argvLength :: Template -> Int
argvLength = Seq.length . argv

envpLength :: Template -> Int
envpLength = Seq.length . envp

addArg :: Int -> ByteString -> Template -> Template
addArg idx newArg t =
  t { argv = Seq.insertAt idx (CString.terminate newArg) (argv t) }

rmArg :: Int -> Template -> Template
rmArg idx t = t { argv = Seq.deleteAt idx (argv t) }

mutArg :: (CString -> CString) -> Int -> Template -> Template
mutArg mut idx t = t { argv = Seq.adjust mut idx (argv t) }

mutArgs :: (Seq CString -> Seq CString) -> Template -> Template
mutArgs mut t = t { argv = mut (argv t) }

mutArgsA :: Functor f => (Seq CString -> f (Seq CString)) -> Template -> f Template
mutArgsA mut t = mut (argv t) <&> \as -> t { argv = as }

-- | Add raw string to env
addEnv :: Int -> ByteString -> Template -> Template
addEnv idx val t =
  t { envp = Seq.insertAt idx (CString.terminate val) (envp t) }

-- | Add key/value env var
addWellFormedEnv :: ByteString -> ByteString -> Template -> Template
addWellFormedEnv var val t =
  t { envp = CString.terminate (var <> "=" <> val) Seq.<| envp t }

rmEnv :: Int -> Template -> Template
rmEnv idx t = t { envp = Seq.deleteAt idx (envp t) }

mutEnv :: (CString -> CString) -> Int -> Template -> Template
mutEnv mut idx t = t { envp = Seq.adjust mut idx (envp t) }

mutEnvs :: (Seq CString -> Seq CString) -> Template -> Template
mutEnvs mut t = t { envp = mut (envp t) }

mutEnvsA :: Functor f => (Seq CString -> f (Seq CString)) -> Template -> f Template
mutEnvsA mut t = mut (envp t) <&> \es -> t { envp = es }

genArgs ::
  C.IsSymBackend sym bak =>
  (wptr ~ ArchWidth arch) =>
  CLLVM.HasPtrWidth wptr =>
  CLLVM.HasLLVMAnn sym =>
  (?memOpts :: CLLVM.MemOptions) =>
  bak ->
  CLLVM.ModuleTranslation arch ->
  C.GlobalVar CLLVM.Mem ->
  Template ->
  C.OverrideSim p sym ext rtp args ret
    (C.RegMap
      sym
      (Ctx.EmptyCtx Ctx.::>
       CLLVM.LLVMPointerType 32 C.::>
       CLLVM.LLVMPointerType (ArchWidth arch) C.::>
       CLLVM.LLVMPointerType (ArchWidth arch)))
genArgs bak trans mvar template = do
  argcVal <- liftIO genArgc
  argvVal <- genArgv "argv" (argv template)
  envpVal <- genArgv "envp" (envp template)
  return $
    C.assignReg CLLVM.PtrRepr envpVal $
      C.assignReg CLLVM.PtrRepr argvVal $
        C.assignReg (CLLVM.LLVMPointerRepr n32) argcVal C.emptyRegMap

  where
    sym = C.backendGetSym bak
    n32 = knownNat @32
    dl =
      CLLVM.llvmDataLayout (trans Lens.^. CLLVM.transContext . CLLVM.llvmTypeCtx)

    genArgc = do
      CLLVM.LLVMPointer
        <$> What4.natLit sym 0
        <*> What4.bvLit sym n32 (BV.mkBV (knownNat @32) (fromIntegral (argvLength template)))

    genArgv which args = do
      let nargv = fromIntegral (Seq.length args)
      let argvType = CLLVM.ArrayType (nargv + 1) (CLLVM.PtrType (CLLVM.MemType CLLVM.i8))
      let argvSize = CLLVM.memTypeSize dl argvType
      let argvAlign = CLLVM.memTypeAlign dl argvType
      argvSizeBv <-
        liftIO $ What4.bvLit sym CLLVM.PtrWidth $ CLLVM.bytesToBV CLLVM.PtrWidth argvSize
      argvPtr <-
        C.modifyGlobal mvar $ \mem ->
          liftIO $ CLLVM.doAlloca bak mem argvSizeBv argvAlign ("czz:" ++ which)

      -- Write null pointer to last index
      argvLastIdx <- foldM (writePtr which) argvPtr args
      nullPtr <- liftIO (CLLVM.mkNullPointer sym ?ptrWidth)
      let strTy =
            CLLVM.bitvectorType (CLLVM.bitsToBytes (natValue CLLVM.PtrWidth))
      let cty = CLLVM.PtrRepr
      C.modifyGlobal mvar $ \mem ->
        ((),) <$> liftIO (CLLVM.doStore bak mem argvLastIdx cty strTy CLLVM.noAlignment nullPtr)

      return argvPtr

    -- Store a pointer to a fresh allocation with string @str@ into @ptr@,
    -- increment @ptr@ by the size of a pointer.
    writePtr which ptr str = do
      strPtr <- newArgvStr which (CString.toByteString str)

      -- Store the new string pointer into the argv array
      let strTy =
            CLLVM.bitvectorType (CLLVM.bitsToBytes (natValue CLLVM.PtrWidth))
      let cty = CLLVM.PtrRepr
      C.modifyGlobal mvar $ \mem ->
        ((),) <$> liftIO (CLLVM.doStore bak mem ptr cty strTy CLLVM.noAlignment strPtr)
      -- Increment the argv pointer by one index
      liftIO $
        CLLVM.ptrAdd sym ?ptrWidth ptr
          =<< What4.bvLit sym ?ptrWidth ptrWidthBv

    -- A bitvector holding the width of a pointer, in bytes
    ptrWidthBv =
      let ptrType = CLLVM.PtrType (CLLVM.MemType CLLVM.i8)
          ptrSize = CLLVM.memTypeSize dl ptrType
      in CLLVM.bytesToBV CLLVM.PtrWidth ptrSize

    -- Allocate space for a string in argv, write string to it
    newArgvStr which str = do
      let val = CLLVM.LLVMValString str
      let ty = CLLVM.llvmValStorableType val
      -- TODO(lb): check for truncation
      let len = BS.length str
      let lenBv = BV.mkBV ?ptrWidth (toEnum len)
      strLen <- liftIO (What4.bvLit sym ?ptrWidth lenBv)

      -- Allocate space for the argument string
      let nm = "czz:" ++ which ++ "-str"
      strPtr <-
        C.modifyGlobal mvar $ \mem ->
          liftIO $ CLLVM.doAlloca bak mem strLen CLLVM.noAlignment nm

      -- Store the argument string into the space for the argument string
      C.modifyGlobal mvar $ \mem ->
        ((),) <$> liftIO (CLLVM.storeRaw bak mem strPtr ty CLLVM.noAlignment val)

      return strPtr
