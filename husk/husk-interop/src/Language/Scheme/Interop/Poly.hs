{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Problem: can't store polymorphic values in 'Dynamic'
--
-- > did :: Dynamic
-- > did = toDyn (id :: forall a. a -> a)
--
-- yields
--
-- >   • No instance for (Typeable a0) arising from a use of ‘toDyn’
-- >   • In the expression: toDyn (id :: forall a. a -> a)
-- >     In an equation for ‘did’: did = toDyn (id :: forall a. a -> a)
-- >  |
-- >  | did = toDyn (id :: forall a. a -> a)
-- >  |       ^^^^^
--
-- More to the point,
--
-- > typeRep @(forall a. a -> a)
--
-- yields
--
-- > • No instance for (Typeable (forall a. a -> a))
-- >     arising from a use of ‘typeRep’
-- > • In the expression: typeRep @(forall a. a -> a)
-- >   In an equation for ‘it’: it = typeRep @(forall a. a -> a)
--
-- Can use a custom type for each polymorphic type...
--
-- > data Id = Id (forall a. a -> a)
-- > getId :: Id -> (forall a. a -> a)
-- > getId (Id f) = f
-- > did :: Dynamic
-- > did = toDyn (Id id)
-- > extractId :: Dynamic -> Maybe (forall a. a -> a)
-- > extractId = fmap getId . fromDynamic
--
-- But then we need to try all the different type schemes to apply the function.
-- What if we could just have one monomorphic (existential) type that pairs a
-- function with a representative of its type scheme?

module Language.Scheme.Interop.Poly
  ( type Kind(..)
  , KindRep(..)
  , type Ctx(..)
  , CtxRep(..)
  , type U(..)
  , URep(..)
  , uCtx
  , uKind
  -- * DeCoderetation
  , DeCode
  , DeCode1'(..)
  , DeCode2'(..)
  , All1(..)
  , All2(..)
  -- * Weakening
  , WeakBy
  , weakBy
  -- * Instantiation
  , inst
  -- * PolyFun
  , PolyFun(..)
  ) where

import           Data.Kind (Type)
import           Data.Functor.Identity (Identity)
import           Data.Proxy (Proxy)
import           Data.Type.Equality (TestEquality(testEquality), (:~:)(Refl))
import qualified Type.Reflection as Reflect

--------------------------------------------------------------------------------
-- Kinds

-- These are just to make it clear when we're talking about kinds (types of
-- types), rather than just types.

-- | As we all know, since @-XTypeInType@ kinds /are/ types.
type Kind = Type

newtype KindRep k = KindRep { getKindRep :: Reflect.TypeRep k }
  deriving (TestEquality)

kTypeRep :: KindRep Type
kTypeRep = KindRep (Reflect.typeRep @Type)

kDomain :: KindRep (k -> l) -> KindRep k
kDomain =
  -- The pattern matches are redundant, but GHC doesn't know that since they're
  -- pattern synonyms.
  \case
    KindRep (Reflect.App (Reflect.App (Reflect.Con _) k) _) -> KindRep k
    KindRep (Reflect.Fun k _) -> KindRep k

kRange :: KindRep (k -> l) -> KindRep l
kRange =
  \case
    KindRep (Reflect.App (Reflect.App (Reflect.Con _) _) l) -> KindRep l
    KindRep (Reflect.Fun _ l) -> KindRep l

--------------------------------------------------------------------------------
-- Contexts

-- | Context of type variables
data Ctx
  = Empty
  | Extend Ctx Kind

-- | We think of contexts as being extended "on the right"
type ctx :> k = 'Extend ctx k

data CtxRep ctx where
  EmptyRep :: CtxRep 'Empty
  (::>) :: CtxRep ctx -> KindRep k -> CtxRep (ctx :> k)

instance TestEquality CtxRep where
  testEquality c d =
    case (c, d) of
      (EmptyRep, EmptyRep) -> Just Refl
      (ctx ::> k, ctx' ::> k') -> do
        Refl <- testEquality ctx ctx'
        Refl <- testEquality k k'
        return Refl
      (_, _) -> Nothing

--------------------------------------------------------------------------------
-- Universe

-- | The universe of types of kind @uk@ that mention variables in @ctx@
--
-- TODO(lb): rename 'code'?
data U (ctx :: Ctx) (uk :: Kind) where
  App :: U ctx (k -> uk) -> U ctx k -> U ctx uk
  Const :: k -> U 'Empty k
  Var :: U (ctx :> uk) uk
  Weak :: U ctx uk -> U (ctx :> k) uk

data URep (ctx :: Ctx) (uk :: Kind) (u :: U ctx uk) :: Type where
  AppRep :: URep ctx (k -> l) f -> URep ctx k u -> URep ctx l ('App f u)
  ConstRep :: KindRep uk -> Reflect.TypeRep f -> URep 'Empty uk ('Const f)
  VarRep :: KindRep uk -> CtxRep ctx -> URep (ctx :> uk) uk 'Var
  WeakRep :: KindRep k -> URep ctx uk u -> URep (ctx :> k) uk ('Weak u)

uCtx :: URep ctx k u -> CtxRep ctx
uCtx =
  \case
    AppRep f _ -> uCtx f
    ConstRep {} -> EmptyRep
    VarRep k ctx -> ctx ::> k
    WeakRep k u -> uCtx u ::> k

uKind :: URep ctx k u -> KindRep k
uKind =
  \case
    AppRep f _a -> kRange (uKind f)
    ConstRep k _ -> k
    VarRep k _ -> k
    WeakRep _ u -> uKind u

instance TestEquality (URep ctx k) where
  testEquality u v =
    case (u, v) of
      (AppRep ku lu, AppRep kv lv) -> do
        Refl <- testEquality (uKind ku) (uKind kv)
        Refl <- testEquality ku kv
        Refl <- testEquality lu lv
        return Refl
      (ConstRep k ty, ConstRep l ty') -> do
        Refl <- testEquality k l
        Refl <- testEquality ty ty'
        return Refl
      (VarRep k ctx, VarRep l ctx') -> do
        Refl <- testEquality k l
        Refl <- testEquality ctx ctx'
        return Refl
      (WeakRep k u', WeakRep l v') -> do
        Refl <- testEquality k l
        Refl <- testEquality u' v'
        return Refl
      (_, _) -> Nothing

--------------------------------------------------------------------------------
-- Decoding

type family EnCode0 (a :: k) :: U 'Empty k where
  EnCode0 (f a) = 'App (EnCode0 f) (EnCode0 a)
  EnCode0 a = 'Const a

type family DeCode0 (u :: U 'Empty uk) :: uk where
  DeCode0 ('App f x) = (DeCode0 f) (DeCode0 x)
  DeCode0 ('Const t) = t

type family DeCode1 (u :: U ('Empty :> k) uk) (a :: k) :: uk where
  DeCode1 ('App f x) a = (DeCode1 f a) (DeCode1 x a)
  DeCode1 'Var a = a
  DeCode1 ('Weak u) a = DeCode0 u

type family
  DeCode2
    (u :: U ('Empty :> k :> l) uk)
    (a :: k)
    (b :: l)
    :: uk where
  DeCode2 ('App f x) a b = (DeCode2 f a b) (DeCode2 x a b)
  DeCode2 ('Weak u) a _ = DeCode1 u a
  DeCode2 'Var _ b = b

data DeCode1'
  (f :: uk -> Type)
  (u :: U ('Empty :> k) uk)
  (a :: k)
  = DeCode1' (f (DeCode1 u a))

data DeCode2'
  (f :: uk -> Type)
  (u :: U ('Empty :> k :> l) uk)
  (a :: k)
  (b :: l)
  = DeCode2' (f (DeCode2 u a b))

data All1 (f :: k -> Type) = All1 (forall (a :: k). f a)
data All2 (f :: k -> l -> Type) = All2 (forall (a :: k) (b :: l). f a b)

-- TODO(lb): currying...?

type family DeCode (ctx :: Ctx) (u :: U ctx uk) (f :: uk -> Type) :: Type where
  DeCode 'Empty u f = f (DeCode0 u)
  DeCode ('Empty :> _) u f = All1 (DeCode1' f u)
  DeCode ('Empty :> _ :> _) u f = All2 (DeCode2' f u)

-- Can't figure out how to avoid having @n@ type families...

-- type family Instantiate (ctx :: Ctx) (u :: U ctx) (l :: [Type]) :: Type where
--   Instantiate ctx (u :--> v) l = Instantiate ctx u l -> Instantiate ctx v l
--   Instantiate 'EmptyCtx ('Const t) '[] = t
--   Instantiate (ctx :> ()) 'Var (x ': xs) = x
--   Instantiate (ctx :> ()) ('Weak u) (x ': xs) = Instantiate ctx u xs

-- -- For partial applications
-- newtype Inst ctx u l = Inst (Instantiate ctx u l)

-- data AllL u l where
--   AllLNil :: Instantiate 'EmptyCtx u '[] -> AllL u '[]
--   AllLCons :: (forall a. Instantiate (ctx :> ()) u (a ': l)) -> AllL u l

-- | Like 'Type.Reflection.TypeRep', but can create representatives of
-- polymorphic types.
data TypeRep (t :: k) where
  TypeRep :: URep ctx k u -> TypeRep (DeCode ctx u Proxy)

instance TestEquality TypeRep where
  testEquality (TypeRep u) (TypeRep v) = do
    Refl <- testEquality (uCtx u) (uCtx v)
    Refl <- testEquality (uKind u) (uKind v)
    Refl <- testEquality u v
    return Refl

class Reflect.Typeable k => Typeable (a :: k) where
  typeRep :: TypeRep a

  -- default typeRep :: Reflect.Typeable a => TypeRep a
  -- typeRep =
  --   TypeRep (ConstRep (kindRep (Proxy @k)) (Reflect.typeRep @a)
  --              :: URep 'Empty (EnKind k) ('Const a))

--------------------------------------------------------------------------------
-- Weakening

type family WeakBy (ctx :: Ctx) (u :: U 'Empty k) :: U ctx k where
  WeakBy 'Empty u = u
  WeakBy (ctx :> _) u = 'Weak (WeakBy ctx u)

weakBy :: CtxRep ctx -> URep 'Empty k u -> URep ctx k (WeakBy ctx u)
weakBy ctx u =
  case (ctx, u) of
    (EmptyRep, _) -> u
    (ctx' ::> k, a@(AppRep {})) -> WeakRep k (weakBy ctx' a)
    (ctx' ::> k, c@(ConstRep {})) -> WeakRep k (weakBy ctx' c)

--------------------------------------------------------------------------------
-- Instantiation

type family
  Inst
    (ctx :: Ctx)
    (u :: U (ctx :> k) uk)
    (a :: k)
    :: U ctx uk where
  Inst ctx ('App f x) a = 'App (Inst ctx f a) (Inst ctx x a)
  Inst ctx 'Var a = WeakBy ctx ('Const a)
  Inst ctx ('Weak u) _ = u

inst :: Reflect.TypeRep a -> URep (ctx :> k) uk u -> URep ctx uk (Inst ctx u a)
inst rep =
  \case
    AppRep f a -> AppRep (inst rep f) (inst rep a)
    VarRep k ctx -> weakBy ctx (ConstRep k rep)
    WeakRep _kRep u -> u

-- TODO(lb): ???
-- data TryInst (ctx :: Ctx) (u :: U ctx) a where
--   InstFail :: TryInst Ctx0 u a
--   InstSucceed ::
--     URep ctx (Inst ctx u a) -> TryInst (ctx :> ()) u a

-- tryInst :: Proxy a -> URep ctx u -> TryInst ctx u a
-- tryInst proxy uRep =
--   case uCtx uRep of
--     ERep -> InstFail
--     XRep ctx -> InstSucceed (inst proxy uRep)

--------------------------------------------------------------------------------
-- Application

-- -- TODO(lb)
-- appMono ::
--   DeCodereted 'Empty u ->
--   DeCode 'Empty u Identity ->
--   URep 'Empty Type v ->
--   DeCode 'Empty v Identity ->

--------------------------------------------------------------------------------
-- Dynamic

-- | 'PolyFun' is the goal: A monomorphic type that captures the polymorphism of
-- the contained function.
data PolyFun where
  PolyFun :: URep ctx Type u -> DeCode ctx u Identity -> PolyFun

--------------------------------------------------------------------------------
-- Examples

type Ctx0 = 'Empty
type Ctx1 = 'Empty :> Type
type Ctx2 = 'Empty :> Type :> Type
-- type a --> b = 'App ('App ('Const (->)) a) b
type a ---> b = 'App ('App ('Weak ('Const (->))) a) b
-- type a ----> b = 'App ('App ('Weak ('Weak ('Const (->)))) a) b

ctx0 :: CtxRep Ctx0
ctx0 = EmptyRep

ctx1 :: CtxRep Ctx1
ctx1 = ctx0 ::> kTypeRep

_var_1_1 :: URep Ctx1 Type 'Var
_var_1_1 = VarRep kTypeRep ctx0

_var_2_1 :: URep Ctx2 Type ('Weak 'Var)
_var_2_1 = WeakRep kTypeRep (VarRep kTypeRep ctx0)

_var_2_2 :: URep Ctx2 Type 'Var
_var_2_2 = VarRep kTypeRep ctx1

type IdType1 = 'Var ---> 'Var

type ArrKind = Type -> Type -> Type

-- arrKindRep :: KindRep ArrKind
-- arrKindRep = kTypeRep :==> kTypeRep :==> kTypeRep

-- arrRep1 :: URep Ctx1 ArrKind ('Weak ('Const (->)))
-- arrRep1 = WeakRep kTypeRep (ConstRep arrKindRep (Reflect.typeRep @(->)))

-- _idRep :: URep Ctx1 Type IdType1
-- _idRep = AppRep (AppRep arrRep1 (VarRep kTypeRep ctx0)) (VarRep kTypeRep ctx0)

-- _idRepWeak :: URep Ctx2 Type ('Weak IdType1)
-- _idRepWeak = WeakRep (VarRep rep0 :---> VarRep rep0)

-- _idRepWeak' :: URep Ctx2 Type ('Weak 'Var ----> 'Weak 'Var)
-- _idRepWeak' = WeakRep var_1_1 :---> WeakRep var_1_1

-- _idInst :: URep Ctx0 Type (Inst Ctx0 IdType1 Bool)
-- _idInst = WeakRep (ConstRep (Proxy @Bool)) :---> WeakRep (ConstRep (Proxy @Bool))

-- _idP :: PolyFun
-- _idP = PolyFun idRep (All1 (DeCode1' id))

-- constRep :: URep Ctx2 Type ('Weak 'Var --> 'Var --> 'Weak 'Var)
-- constRep = var_2_1 :---> var_2_2 :---> var_2_1

-- _constP :: PolyFun
-- _constP = PolyFun constRep (All2 (DeCode2' const))

-- _getConst ::
--   All2 (DeCode2' ('Weak 'Var --> 'Var --> 'Weak 'Var)) ->
--   (forall a b. a -> b -> a)
-- _getConst (All2 (DeCode2' const_)) = const_
