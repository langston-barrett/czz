{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Czz.LLVM.Script.Val
  ( extendEnv
  , SVal(..)
  , testSym
  , bool
  , i1
  , i8
  , i16
  , i32
  , i64
  , expr
  ) where

import           Control.Monad.Except (ExceptT(..))  -- for auto
import           Control.Monad.IO.Class (liftIO)
import qualified Data.BitVector.Sized as BV
import           Data.Type.Equality ((:~:)(Refl), testEquality)

import           Data.Parameterized.NatRepr (NatRepr, type (<=))
import qualified Data.Parameterized.NatRepr as NatRepr
import           Data.Parameterized.Nonce (GlobalNonceGenerator, Nonce)
import qualified Data.Parameterized.Nonce as Nonce

import qualified What4.Interface as What4

import qualified Lang.Crucible.Simulator as C

import qualified Lang.Crucible.LLVM.MemModel as CLLVM

import qualified Language.Scheme.Types as LST

import           Language.Scheme.CustFunc (CustFunc)
import qualified Language.Scheme.CustFunc as Cust
import           Language.Scheme.Opaque (Opaque(..))  -- for auto

import           Language.Scheme.What4 (SExpr, SExprBuilder)
import qualified Language.Scheme.What4 as SWhat4

extendEnv :: String -> LST.Env -> IO LST.Env
extendEnv pfx e = do
  Cust.extendEnv funcs pfx e
  where
    funcs =
      [ expr
      , bool
      , i1
      , i8
      , i16
      , i32
      , i64
      ]

data SVal where
  -- TODO(lb): also pointers
  SVal ::
    Nonce Nonce.GlobalNonceGenerator sym ->
    C.RegEntry sym tp ->
    SVal

testSym ::
  Nonce GlobalNonceGenerator sym ->
  Nonce GlobalNonceGenerator sym' ->
  IO (sym :~: sym')
testSym nsym nsym' =
  case testEquality nsym nsym' of
    Nothing -> fail "Mismatched ExprBuilder"
    Just Refl -> return Refl

bool :: CustFunc
bool =
  Cust.CustFunc
  { Cust.custFuncName = "bool"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto impl)
  }
  where
    impl :: SExprBuilder -> Bool -> LST.IOThrowsError SVal
    impl exprBuilder b = liftIO $ do
      SWhat4.SExprBuilder sym nsym <- return exprBuilder
      let one = NatRepr.knownNat @1
      let bv = BV.mkBV one (if b then 1 else 0)
      ptr <- liftIO (CLLVM.llvmPointer_bv sym =<< What4.bvLit sym one bv)
      return (SVal nsym (C.RegEntry (CLLVM.LLVMPointerRepr one) ptr))

intLit :: (1 <= n) => NatRepr n -> CustFunc
intLit n =
  Cust.CustFunc
  { Cust.custFuncName = "i" ++ show (NatRepr.natValue n)
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto impl)
  }
  where
    impl :: SExprBuilder -> Integer -> LST.IOThrowsError SVal
    impl exprBuilder i = liftIO $ do
      SWhat4.SExprBuilder sym nsym <- return exprBuilder
      let bv = BV.mkBV n i
      ptr <- liftIO (CLLVM.llvmPointer_bv sym =<< What4.bvLit sym n bv)
      return (SVal nsym (C.RegEntry (CLLVM.LLVMPointerRepr n) ptr))

i1 :: CustFunc
i1 = intLit (NatRepr.knownNat @1)

i8 :: CustFunc
i8 = intLit (NatRepr.knownNat @8)

i16 :: CustFunc
i16 = intLit (NatRepr.knownNat @16)

i32 :: CustFunc
i32 = intLit (NatRepr.knownNat @32)

i64 :: CustFunc
i64 = intLit (NatRepr.knownNat @64)

expr :: CustFunc
expr =
  Cust.CustFunc
  { Cust.custFuncName = "expr"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto impl)
  }
  where
    impl :: SExprBuilder -> SExpr -> LST.IOThrowsError SVal
    impl exprBuilder =
      \case
        SWhat4.SBV nsym w bvExpr -> do
          SWhat4.SExprBuilder sym nsym' <- return exprBuilder
          Refl <- liftIO (testSym nsym nsym')
          ptr <- liftIO (CLLVM.llvmPointer_bv sym bvExpr)
          return (SVal nsym (C.RegEntry (CLLVM.LLVMPointerRepr w) ptr))
