{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Scheme.What4
  ( extendEnv
  , bvLit
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.BitVector.Sized as BV

import           Data.Parameterized.NatRepr (NatRepr)
import qualified Data.Parameterized.NatRepr as NatRepr
import           Data.Parameterized.Nonce (Nonce)
import qualified Data.Parameterized.Nonce as Nonce
import           Data.Parameterized.Some (Some(Some))

import           What4.Interface (IsSymExprBuilder, SymExpr)
import qualified What4.Interface as What4

import qualified Language.Scheme.Types as LST

import           Language.Scheme.CustFunc (CustFunc)
import qualified Language.Scheme.CustFunc as Cust
import           Language.Scheme.Opaque (Opaque(..))  -- for auto

extendEnv ::
  IsSymExprBuilder sym =>
  sym ->
  String ->
  LST.Env ->
  IO LST.Env
extendEnv sym pfx e = do
  symNonce <- Nonce.freshNonce Nonce.globalNonceGenerator
  Cust.extendEnv (funcs symNonce) pfx e
  where
    funcs symNonce =
      [ bvLit sym symNonce
      ]

-- | Helper, not exported
lift :: IO a -> LST.IOThrowsError a
lift = liftIO
{-# INLINE lift #-}

data SomeExpr where
  SomeBV ::
    IsSymExprBuilder sym =>
    Nonce Nonce.GlobalNonceGenerator sym ->
    NatRepr w ->
    SymExpr sym (What4.BaseBVType w) ->
    SomeExpr

bvLit ::
  forall sym.
  IsSymExprBuilder sym =>
  sym ->
  Nonce Nonce.GlobalNonceGenerator sym ->
  CustFunc
bvLit sym symNonce =
  Cust.CustFunc
  { Cust.custFuncName = "bvLit"
  , Cust.custFuncImpl =
      Cust.evalHuskable (Cust.auto (\w i -> lift (impl w i)))
  }
  where
    impl :: Integer -> Integer -> IO SomeExpr
    impl w i = do
      -- TODO(lb): can fail
      Some wRepr <- return (NatRepr.mkNatRepr (fromIntegral w))
      case NatRepr.isZeroOrGT1 wRepr of
        Left NatRepr.Refl -> fail "Bad bitvector width"
        Right NatRepr.LeqProof -> do
          bv <- What4.bvLit sym wRepr (BV.mkBV wRepr i)
          return (SomeBV symNonce wRepr bv)
