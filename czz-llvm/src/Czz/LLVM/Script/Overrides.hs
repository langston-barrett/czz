{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Czz.LLVM.Script.Overrides
  ( extendEnv
  , SMem(..)
  , SOverride(..)
  , registerSOverrides
  , override
  ) where

import qualified Control.Monad.Except as Exc
import qualified Control.Lens as Lens
import           Control.Monad.Except (ExceptT(..))  -- for auto
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import           Data.Type.Equality (testEquality, (:~:)(Refl))

import qualified Text.LLVM.AST as L

import           Data.Parameterized.Nonce (Nonce)
import qualified Data.Parameterized.Nonce as Nonce
import           Data.Parameterized.TraversableFC (toListFC)

-- crucible
import           Lang.Crucible.Backend (IsSymInterface)
import qualified Lang.Crucible.Simulator as C

-- crucible-llvm
import           Lang.Crucible.LLVM.Extension (ArchWidth)
import           Lang.Crucible.LLVM.Intrinsics (OverrideTemplate)
import qualified Lang.Crucible.LLVM.Intrinsics as CLLVM
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import qualified Lang.Crucible.LLVM.Translation as CLLVM

import           Data.Dyn1 (Dyn1)
import qualified Data.Dyn1 as Dyn1

import qualified Language.Scheme.Types as LST

import           Language.Scheme.CustFunc (CustFunc)
import qualified Language.Scheme.CustFunc as Cust
import           Language.Scheme.Opaque (Opaque(..))  -- for auto

import           Language.Scheme.What4 (SExprBuilder)
import qualified Language.Scheme.What4 as SWhat4

import           Czz.LLVM.Translate (Translation)
import qualified Czz.LLVM.Translate as Trans
import           Czz.LLVM.Script.Val (SVal)
import qualified Czz.LLVM.Script.Val as SVal

extendEnv :: String -> LST.Env -> IO LST.Env
extendEnv pfx e = do
  Cust.extendEnv funcs pfx e
  where
    funcs =
      [ override
      ]

-- | Helper, not exported
_lift :: IO a -> LST.IOThrowsError a
_lift = liftIO
{-# INLINE _lift #-}

-- | Helper, not exported
_lift1 :: (a -> IO b) -> a -> LST.IOThrowsError b
_lift1 f a = liftIO (f a)
{-# INLINE _lift1 #-}

-- | Helper, not exported
_lift2 :: (a -> b -> IO c) -> a -> b -> LST.IOThrowsError c
_lift2 f a b = liftIO (f a b)
{-# INLINE _lift2 #-}

-- | Helper, not exported
_lift3 :: (a -> b -> c -> IO d) -> a -> b -> c -> LST.IOThrowsError d
_lift3 f a b c = liftIO (f a b c)
{-# INLINE _lift3 #-}

data SMem where
  SMem ::
    Nonce Nonce.GlobalNonceGenerator sym ->
    CLLVM.MemImpl sym ->
    SMem

data SOverride where
  SOverride ::
    (forall proxy arch sym wptr p rtp l a.
     ( IsSymInterface sym
     , (wptr ~ ArchWidth arch)
     , CLLVM.HasPtrWidth wptr
     , CLLVM.HasLLVMAnn sym
     ) =>
     proxy arch ->
     sym ->
     Nonce Nonce.GlobalNonceGenerator sym ->
     OverrideTemplate p sym arch rtp l a) ->
    SOverride

registerSOverrides ::
  IsSymInterface sym =>
  (wptr ~ ArchWidth arch) =>
  CLLVM.HasPtrWidth wptr =>
  CLLVM.HasLLVMAnn sym =>
  (?memOpts :: CLLVM.MemOptions) =>
  (?intrinsicsOpts :: CLLVM.IntrinsicsOptions) =>
  sym ->
  Nonce Nonce.GlobalNonceGenerator sym ->
  CLLVM.ModuleTranslation arch ->
  [SOverride] ->
  C.OverrideSim p sym CLLVM.LLVM rtp args ret ()
registerSOverrides sym nsym trans ovs = do
  let llvmAst = trans Lens.^. CLLVM.modTransModule
  let llvmCtx = trans Lens.^. CLLVM.transContext
  CLLVM.register_llvm_overrides llvmAst (map get ovs) [] llvmCtx
  where get (SOverride s) = s trans sym nsym

override :: CustFunc
override =
  Cust.CustFunc
  { Cust.custFuncName = "override"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto impl)
  }
  where
    impl ::
      Translation ->
      String ->
      (SExprBuilder -> SMem -> [SVal] -> LST.IOThrowsError (SMem, SVal)) ->
      Dyn1 Maybe
    impl t name f = (Dyn1.to :: Maybe SOverride -> Dyn1 Maybe) $ do
      Trans.Translation trans _memVar _entry <- return t
      let llvmAst = trans Lens.^. CLLVM.modTransModule
      let decls = Map.fromList (map (\d -> (L.defName d, CLLVM.declareFromDefine d)) (L.modDefines llvmAst))
      decl <- Map.lookup (L.Symbol name) decls
      let llvmCtx = trans Lens.^. CLLVM.transContext
      let ?lc = llvmCtx Lens.^. CLLVM.llvmTypeCtx
      CLLVM.llvmPtrWidth llvmCtx $ \ptrW -> CLLVM.withPtrWidth ptrW $ do
        CLLVM.llvmDeclToFunHandleRepr' decl $ \argTys retTy -> Just $
          SOverride $ \_proxy sym nsym ->
            CLLVM.basic_llvm_override $
              CLLVM.LLVMOverride
                { CLLVM.llvmOverride_declare = decl,
                  CLLVM.llvmOverride_args = argTys,
                  CLLVM.llvmOverride_ret = retTy,
                  CLLVM.llvmOverride_def = \memVar _bak cArgs -> do
                    C.modifyGlobal memVar $ \mem -> do
                      -- TODO(lb): fails should log an error
                      let ssym = SWhat4.SExprBuilder sym nsym
                      let smem = SMem nsym mem
                      let args = toListFC (SVal.SVal nsym) cArgs
                      liftIO (Exc.runExceptT (f ssym smem args)) >>=
                        \case
                          Left err -> fail (show err)
                          Right (SMem nmem mem', SVal.SVal nsym' ret) -> do
                            Refl <-
                              case testEquality nsym nsym' of
                                Just r -> return r
                                Nothing -> fail "TODO"
                            Refl <-
                              case testEquality nsym nmem of
                                Just r -> return r
                                Nothing -> fail "TODO"
                            -- TODO(lb): error messages!
                            Refl <-
                              case testEquality (C.regType ret) retTy of
                                Just r -> return r
                                Nothing ->
                                  fail $
                                    unlines
                                      [ "Bad return type"
                                      , "Expected: " ++ show retTy
                                      , "Found: " ++ show (C.regType ret)
                                      ]
                            return (C.regValue ret, mem')
                }


