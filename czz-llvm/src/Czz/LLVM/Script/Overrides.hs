{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Czz.LLVM.Script.Overrides
  ( extendEnv
  , fromWhat4
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
import qualified Lang.Crucible.Types as C

-- crucible-llvm
import           Lang.Crucible.LLVM.Extension (ArchWidth)
import           Lang.Crucible.LLVM.Intrinsics (OverrideTemplate)
import qualified Lang.Crucible.LLVM.Intrinsics as CLLVM
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import qualified Lang.Crucible.LLVM.Translation as CLLVM

import qualified Language.Scheme.Types as LST

import           Language.Scheme.CustFunc (CustFunc)
import qualified Language.Scheme.CustFunc as Cust
import           Language.Scheme.Opaque (Opaque(..))  -- for auto

import           Language.Scheme.What4 (SExpr)
import qualified Language.Scheme.What4 as SWhat4

import           Czz.LLVM.Translate (Translation)
import qualified Czz.LLVM.Translate as Trans

extendEnv ::
  IsSymInterface sym =>
  sym ->
  Nonce Nonce.GlobalNonceGenerator sym ->
  String ->
  LST.Env ->
  IO LST.Env
extendEnv sym nsym pfx e = do
  Cust.extendEnv funcs pfx e
  where
    funcs =
      [ fromWhat4 sym nsym
      , override sym nsym
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
    Nonce Nonce.GlobalNonceGenerator sym ->
    (forall arch wptr p rtp l a.
     ( IsSymInterface sym
     , (wptr ~ ArchWidth arch)
     , CLLVM.HasPtrWidth wptr
     , CLLVM.HasLLVMAnn sym
     ) =>
     OverrideTemplate p sym arch rtp l a) ->
    SOverride

data SVal where
  -- TODO(lb): also pointers
  SVal ::
    Nonce Nonce.GlobalNonceGenerator sym ->
    C.RegEntry sym tp ->
    SVal

fromWhat4 ::
  IsSymInterface sym =>
  sym ->
  Nonce Nonce.GlobalNonceGenerator sym ->
  CustFunc
fromWhat4 _sym nsym =
  Cust.CustFunc
  { Cust.custFuncName = "from-what4"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto impl)
  }
  where
    impl :: SExpr -> LST.IOThrowsError SVal
    impl =
      \case
        SWhat4.SBV nsym' w bvExpr ->
          case testEquality nsym nsym' of
            -- TODO(lb): error message, panic?
            Nothing -> fail "Mismatched symbolic backends..?"
            Just Refl ->
              return (SVal nsym (C.RegEntry (C.BVRepr w) bvExpr))


override ::
  IsSymInterface sym =>
  sym ->
  Nonce Nonce.GlobalNonceGenerator sym ->
  CustFunc
override _sym nsym =
  Cust.CustFunc
  { Cust.custFuncName = "override"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto impl)
  }
  where
    impl ::
      Translation ->
      String ->
      ((SMem, [SVal]) -> LST.IOThrowsError (SMem, SVal)) ->
      Maybe SOverride
    impl t name f = do
      Trans.Translation trans _memVar _entry <- return t
      let llvmAst = trans Lens.^. CLLVM.modTransModule
      let decls = Map.fromList (map (\d -> (L.defName d, CLLVM.declareFromDefine d)) (L.modDefines llvmAst))
      decl <- Map.lookup (L.Symbol name) decls
      let llvmCtx = trans Lens.^. CLLVM.transContext
      let ?lc = llvmCtx Lens.^. CLLVM.llvmTypeCtx
      CLLVM.llvmPtrWidth llvmCtx $ \ptrW -> CLLVM.withPtrWidth ptrW $ do
        CLLVM.llvmDeclToFunHandleRepr' decl $ \argTys retTy -> Just $
          SOverride nsym $
            CLLVM.basic_llvm_override $
              CLLVM.LLVMOverride
                { CLLVM.llvmOverride_declare = decl,
                  CLLVM.llvmOverride_args = argTys,
                  CLLVM.llvmOverride_ret = retTy,
                  CLLVM.llvmOverride_def = \memVar _sym cArgs -> do
                    C.modifyGlobal memVar $ \mem -> do
                      let args = toListFC (SVal nsym) cArgs
                      liftIO (Exc.runExceptT (f (SMem nsym mem, args))) >>=
                        \case
                          Left err -> fail (show err)
                          Right (SMem nmem mem', SVal nsym' ret) -> do
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
                                Nothing -> fail "Bad return type"
                            return (C.regValue ret, mem')
                }


