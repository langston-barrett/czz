{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TestPoly (tests) where

import           Data.Kind (Type)
import           Data.Maybe (isJust, isNothing)
import qualified Type.Reflection as R
import           Data.Type.Equality (testEquality)

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as TastyH

import           Language.Scheme.Interop.Poly

tests :: Tasty.TestTree
tests =
  Tasty.testGroup "Poly"
    [ TastyH.testCase "const ==U const" $ do
        TastyH.assertBool "" (isJust (testEquality constURep constURep))
    , TastyH.testCase "id ==U id" $ do
        TastyH.assertBool "" (isJust (testEquality idURep idURep))
    , TastyH.testCase "const /=U id" $ do
        TastyH.assertBool "" (isNothing (testEquality constURep (weak1 idURep)))
    , TastyH.testCase "const ==V const" $ do
        TastyH.assertBool "" (isJust (testEquality constRep constRep))
    , TastyH.testCase "id ==V id" $ do
        TastyH.assertBool "" (isJust (testEquality idRep idRep))
    , TastyH.testCase "5 == (fromDyn' idDyn' :: forall a. a -> a) 5" $ do
        case fromDyn' idRep' idDyn' :: Maybe (DecodeV' Ctx1 ('Forall' IdType1)) of
          Nothing -> error "fail!"
          Just (DecodeV' f) -> 5 TastyH.@=? f (EEmptyRep ::& R.typeRep @Int) 5
    , TastyH.testCase "isNothing (fromDyn' idDyn' :: ())" $ do
        TastyH.assertBool "" $
          isNothing $ (fromDyn' unitRep' idDyn')
    , TastyH.testCase "isNothing (fromDyn' idDyn' :: () -> ())" $ do
        TastyH.assertBool "" $
          isNothing $ (fromDyn' unitRep' idDyn')
    ]

--------------------------------------------------------------------------------
-- Helpers

type Ctx0 = 'Empty
type Ctx1 = 'Empty ':> Type
type Ctx2 = 'Empty ':> Type ':> Type
-- type a --> b = 'App ('App ('Const (->)) a) b
type a ---> b = 'App ('App ('Weak ('Const (->))) a) b
type a ----> b = 'App ('App ('Weak ('Weak ('Const (->)))) a) b

ctx0 :: CtxRep Ctx0
ctx0 = EmptyRep

ctx1 :: CtxRep Ctx1
ctx1 = ctx0 ::> kTypeRep

ctx2 :: CtxRep Ctx2
ctx2 = ctx1 ::> kTypeRep

var_1_1 :: URep Ctx1 Type 'Var
var_1_1 = VarRep kTypeRep ctx0

var_2_1 :: URep Ctx2 Type ('Weak 'Var)
var_2_1 = WeakRep kTypeRep (VarRep kTypeRep ctx0)

var_2_2 :: URep Ctx2 Type 'Var
var_2_2 = VarRep kTypeRep ctx1

type IdType1 = 'Var ---> 'Var

-- type ArrKind = Type -> Type -> Type

-- arrKindRep :: KindRep ArrKind
-- arrKindRep = kTypeRep :==> kTypeRep :==> kTypeRep

-- arrRep1 :: URep Ctx1 ArrKind ('Weak ('Const (->)))
-- arrRep1 = WeakRep kTypeRep (ConstRep arrKindRep (R.typeRep @(->)))

weak1 :: URep ctx k u -> URep (ctx ':> Type) k ('Weak u)
weak1 = WeakRep kTypeRep

weak2 :: URep ctx k u -> URep (ctx ':> Type ':> Type) k ('Weak ('Weak u))
weak2 = weak1 . weak1

arrRep0 :: URep Ctx0 (Type -> Type -> Type) ('Const (->))
arrRep0 = ConstRep (R.typeRep @(->))

arrRep1 :: URep Ctx1 (Type -> Type -> Type) ('Weak ('Const (->)))
arrRep1 = weak1 arrRep0

arrRep2 :: URep Ctx2 (Type -> Type -> Type) ('Weak ('Weak ('Const (->))))
arrRep2 = weak2 arrRep0

--------------------------------------------------------------------------------
-- Examples

unitURep :: URep 'Empty Type (Encode 'Empty ())
unitURep = ConstRep (R.typeRep @())

unitRep' :: PolytypeRep' (DecodeV' 'Empty ('Forall' (Encode 'Empty ())))
unitRep' = PolytypeRep' (ForallRep' EmptyRep unitURep)

unitIdURep :: URep 'Empty Type (Encode 'Empty (() -> ()))
unitIdURep = (AppRep (AppRep (ConstRep (R.typeRep @(->))) unitURep) unitURep)

unitIdRep' :: PolytypeRep' (DecodeV' 'Empty ('Forall' (Encode 'Empty (() -> ()))))
unitIdRep' = PolytypeRep' (ForallRep' EmptyRep unitIdURep)

idURep :: URep Ctx1 Type IdType1
idURep = AppRep (AppRep arrRep1 var_1_1) var_1_1

idRep :: PolytypeRep (DecodeV 'Empty 'EEmpty ('Forall Type ('Base IdType1)))
idRep = PolytypeRep (ForallRep kTypeRep (BaseRep idURep))

idRep' :: PolytypeRep' (DecodeV' Ctx1 ('Forall' IdType1))
idRep' = PolytypeRep' (ForallRep' ctx1 idURep)

idDyn :: Dynamic
idDyn = toDyn idRep (DecodeForall (DecodeBase id))

idDyn' :: Dynamic'
idDyn' = toDyn' idRep' (DecodeV' (\(EEmptyRep ::& _) -> id))

-- _idRepWeak :: URep Ctx2 Type ('Weak IdType1)
-- _idRepWeak = WeakRep (VarRep rep0 :---> VarRep rep0)

-- _idRepWeak' :: URep Ctx2 Type ('Weak 'Var ----> 'Weak 'Var)
-- _idRepWeak' = WeakRep var_1_1 :---> WeakRep var_1_1

-- _idInst :: URep Ctx0 Type (Inst Ctx0 IdType1 Bool)
-- _idInst = WeakRep (ConstRep (Proxy @Bool)) :---> WeakRep (ConstRep (Proxy @Bool))

-- _idP :: PolyFun
-- _idP = PolyFun idRep (All1 (DeU1' id))

type ConstType = 'Weak 'Var ----> 'Var ----> 'Weak 'Var

constURep :: URep Ctx2 Type ConstType
constURep =
  AppRep (AppRep arrRep2 (AppRep (AppRep arrRep2 var_2_1) var_2_2)) var_2_1

constRep :: PolytypeRep (DecodeV 'Empty 'EEmpty ('Forall Type ('Forall Type ('Base ConstType))))
constRep = PolytypeRep (ForallRep kTypeRep (ForallRep kTypeRep (BaseRep constURep)))

constRep' :: PolytypeRep' (DecodeV' Ctx2 ('Forall' ConstType))
constRep' = PolytypeRep' (ForallRep' ctx2 constURep)

-- constDyn :: Dynamic
-- constDyn =
--   toDyn constRep $
--     DecodeForall $
--       DecodeForall $
--         (DecodeBase _ ::
--           forall (a :: Type) (b :: Type).
--           DecodeV ('Empty :> Type :> Type) ('EEmpty :& a :& b) ('Base ConstType))

-- constDyn' :: Dynamic'
-- constDyn' =
--   toDyn' constRep $
--     DecodeForall $
--       DecodeForall $
--         (DecodeBase _ ::
--           forall (a :: Type) (b :: Type).
--           DecodeV ('Empty :> Type :> Type) ('EEmpty :& a :& b) ('Base ConstType))

-- _constP :: PolyFun
-- _constP = PolyFun constRep (All2 (DeU2' const))

-- _getConst ::
--   All2 (DeU2' ('Weak 'Var --> 'Var --> 'Weak 'Var)) ->
--   (forall a b. a -> b -> a)
-- _getConst (All2 (DeU2' const_)) = const_
