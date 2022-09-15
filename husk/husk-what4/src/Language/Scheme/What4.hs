{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Language.Scheme.What4
  ( extendEnv
  , SExprBuilder(..)
  , SExpr(..)
  , bvLit
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.BitVector.Sized as BV
import           Control.Monad.Except (ExceptT(..))  -- for auto

import           Data.Parameterized.NatRepr (NatRepr)
import qualified Data.Parameterized.NatRepr as NatRepr
import           Data.Parameterized.Nonce (Nonce)
import qualified Data.Parameterized.Nonce as Nonce
import           Data.Parameterized.Some (Some(Some))

import           What4.InterpretedFloatingPoint (IsInterpretedFloatSymExprBuilder)
import           What4.Interface (IsSymExprBuilder, SymExpr)
import qualified What4.Interface as What4

import qualified Language.Scheme.Types as LST

import qualified Language.Scheme.Interop.To.Func.Auto as IAuto
import qualified Language.Scheme.Interop.To.Func as ToFunc
import           Language.Scheme.Interop.CustFunc (CustFunc)
import qualified Language.Scheme.Interop.CustFunc as Cust
import           Language.Scheme.Interop.Opaque (Opaque(..))  -- for auto

extendEnv :: String -> LST.Env -> IO LST.Env
extendEnv pfx e = Cust.extendEnv funcs pfx e
  where
    funcs =
      [ bvLit
      ]

-- | Helper, not exported
lift :: IO a -> LST.IOThrowsError a
lift = liftIO
{-# INLINE lift #-}

data SExprBuilder where
  SExprBuilder ::
    ( IsSymExprBuilder sym
    , IsInterpretedFloatSymExprBuilder sym
    ) =>
    sym ->
    Nonce Nonce.GlobalNonceGenerator sym ->
    SExprBuilder

data SExpr where
  SBV ::
    (1 NatRepr.<= w) =>
    Nonce Nonce.GlobalNonceGenerator sym ->
    NatRepr w ->
    SymExpr sym (What4.BaseBVType w) ->
    SExpr

bvLit :: CustFunc
bvLit =
  Cust.CustFunc
  { Cust.custFuncName = "bv-lit"
  , Cust.custFuncImpl = ToFunc.toSchemeFunc (IAuto.auto impl)
  }
  where
    impl :: SExprBuilder -> Integer -> Integer -> LST.IOThrowsError SExpr
    impl sExprBuilder w i = lift $ do
      SExprBuilder sym nsym <- return sExprBuilder
      -- TODO(lb): can fail
      Some wRepr <- return (NatRepr.mkNatRepr (fromIntegral w))
      case NatRepr.isZeroOrGT1 wRepr of
        Left NatRepr.Refl -> fail "Bad bitvector width"
        Right NatRepr.LeqProof -> do
          bv <- What4.bvLit sym wRepr (BV.mkBV wRepr i)
          return (SBV nsym wRepr bv)
