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
  ( -- * Kinds
    KindRep(..)
    -- * Contexts
  , type Ctx(..)
  , CtxRep(..)
  , type Code(..)
  , CodeRep(..)
  , codeCtx
  , codeKind
  -- * Encoding
  , EnCode
  , enCodeInj
  -- * Decoding
  , DeCode
  , DeCode1'(..)
  , DeCode2'(..)
  , All(..)
  -- * Weakening
  , WeakBy
  , weakBy
  -- * Instantiation
  , inst
  -- * PolyFun
  , PolyFun(..)
  ) where

import           Data.Kind (Type)
import           Data.Proxy (Proxy(Proxy))
import           Data.Functor.Identity (Identity)
import           Data.Type.Equality (TestEquality(testEquality), (:~:)(Refl))
import qualified Type.Reflection as Reflect
import           Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- Kinds

-- These are just to make it clear when we're talking about kinds (types of
-- types), rather than just types.

-- | As we all know, since @-XTypeInType@ kinds /are/ types.
type Kind = Type

newtype KindRep k = KindRep { _getKindRep :: Reflect.TypeRep k }
  deriving (TestEquality)

kindRep :: forall a. Reflect.Typeable a => KindRep a
kindRep = KindRep (Reflect.typeRep @a)

typeRepKind :: forall k (a :: k). Reflect.TypeRep a -> KindRep k
typeRepKind a = KindRep (Reflect.typeRepKind a)

kTypeRep :: KindRep Type
kTypeRep = KindRep (Reflect.typeRep @Type)

_kDomain :: KindRep (k -> l) -> KindRep k
_kDomain =
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

data Assign (ctx :: Ctx) where
  AssignEmpty :: Assign 'Empty
  (:::>) ::
    forall ctx k a.
    Assign ctx ->
    k ->
    Assign (ctx :> k)

data AssignRep (as :: Assign ctx) where
  AnEmptyRep :: AssignRep 'AssignEmpty
  AnExtendRep ::
    AssignRep as ->
    Reflect.TypeRep (a :: k) ->
    AssignRep (as ':::> a)

-- instance TestEquality AssignRep where
--   testEquality x y =
--     case (x, y) of
--       (AnEmptyRep, AnEmptyRep) -> Just Refl
--       (AnExtendRep x' t, AnExtendRep y' u) -> do
--         Refl <- testEquality x' y'
--         Refl <- testEquality (typeRepKind t) (typeRepKind u)
--         Refl <- testEquality t u
--         return _

--------------------------------------------------------------------------------
-- Codeniverse

-- | Descriptions of the universe of types of kind @uk@ that mention variables
-- in @ctx@.
data Code (ctx :: Ctx) (uk :: Kind) where
  App :: Code ctx (k -> uk) -> Code ctx k -> Code ctx uk
  Const :: k -> Code 'Empty k
  Var :: Code (ctx :> uk) uk
  Weak :: Code ctx uk -> Code (ctx :> k) uk

data CodeRep (ctx :: Ctx) (uk :: Kind) (u :: Code ctx uk) :: Type where
  AppRep :: CodeRep ctx (k -> l) f -> CodeRep ctx k u -> CodeRep ctx l ('App f u)
  ConstRep :: Reflect.TypeRep c -> CodeRep 'Empty uk ('Const c)
  VarRep :: KindRep uk -> CtxRep ctx -> CodeRep (ctx :> uk) uk 'Var
  WeakRep :: KindRep k -> CodeRep ctx uk u -> CodeRep (ctx :> k) uk ('Weak u)

codeCtx :: CodeRep ctx k u -> CtxRep ctx
codeCtx =
  \case
    AppRep f _ -> codeCtx f
    ConstRep {} -> EmptyRep
    VarRep k ctx -> ctx ::> k
    WeakRep k u -> codeCtx u ::> k

codeKind :: CodeRep ctx k u -> KindRep k
codeKind =
  \case
    AppRep f _a -> kRange (codeKind f)
    ConstRep t -> typeRepKind t
    VarRep k _ -> k
    WeakRep _ u -> codeKind u

instance TestEquality (CodeRep ctx u) where
  testEquality u v =
    case (u, v) of
      (AppRep ku lu, AppRep kv lv) -> do
        Refl <- testEquality (codeKind ku) (codeKind kv)
        Refl <- testEquality ku kv
        Refl <- testEquality lu lv
        return Refl
      (ConstRep ty, ConstRep ty') -> do
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
-- Encoding

type family EnCode (ctx :: Ctx) (a :: k) = (u :: Code ctx k) where
  EnCode ctx (f a) = 'App (EnCode ctx f) (EnCode ctx a)
  EnCode 'Empty a = 'Const a
  EnCode (ctx :> _) a = 'Weak (EnCode ctx a)

data EnCoded a where
  EnCodedApp ::
    (a ~ f b) =>
    Reflect.TypeRep f ->
    Reflect.TypeRep b ->
    (forall ctx. EnCode ctx a :~: 'App (EnCode ctx f) (EnCode ctx b)) ->
    EnCoded a
  EnCodedCon ::
    (EnCode 'Empty a ~ 'Const a) =>
    (forall ctx k.
     KindRep k ->
     CtxRep (ctx :> k) ->
     EnCode (ctx :> k) a :~: 'Weak (EnCode ctx a)) ->
    EnCoded a

enCoded :: forall a. Reflect.TypeRep a -> EnCoded a
enCoded =
  \case
    Reflect.Con _ ->
      -- TODO(lb): Justify!
      case unsafeCoerce Refl :: EnCode 'Empty a :~: 'Const a of
        Refl -> EnCodedCon (\_k _c -> (unsafeCoerce Refl :: EnCode (ctx :> k) a :~: 'Weak (EnCode ctx a)))
    Reflect.App f x -> EnCodedApp f x Refl

appInj1 :: 'App f a :~: 'App g b -> f :~: g
appInj1 Refl = Refl

appInj2 :: 'App f a :~: 'App g b -> a :~: b
appInj2 Refl = Refl

enCodeInjAppOuter ::
  forall f a g b ctx.
  CtxRep ctx ->
  Reflect.TypeRep f ->
  Reflect.TypeRep a ->
  Reflect.TypeRep g ->
  Reflect.TypeRep b ->
  EnCode ctx (f a) :~: EnCode ctx (g b) ->
  f :~: g
enCodeInjAppOuter ctx f a g b Refl = enCodeInj ctx f g (appInj1 r)
  where
    r :: 'App (EnCode ctx g) (EnCode ctx b) :~: 'App (EnCode ctx f) (EnCode ctx a)
    r = Refl

enCodeInjAppInner ::
  forall f a g b ctx.
  CtxRep ctx ->
  Reflect.TypeRep f ->
  Reflect.TypeRep a ->
  Reflect.TypeRep g ->
  Reflect.TypeRep b ->
  EnCode ctx (f a) :~: EnCode ctx (g b) ->
  a :~: b
enCodeInjAppInner ctx f a g b Refl = enCodeInj ctx a b (appInj2 r)
  where
    r :: 'App (EnCode ctx g) (EnCode ctx b) :~: 'App (EnCode ctx f) (EnCode ctx a)
    r = Refl

enCodeInj ::
  forall a b ctx.
  CtxRep ctx ->
  Reflect.TypeRep a ->
  Reflect.TypeRep b ->
  EnCode ctx a :~: EnCode ctx b ->
  a :~: b
enCodeInj ctx a b r@Refl =
  case (enCoded a, enCoded b) of
    (EnCodedApp f a' Refl, EnCodedApp g b' Refl) ->
      case (enCodeInjAppOuter ctx f a' g b' r, enCodeInjAppInner ctx f a' g b' r) of
        (Refl, Refl) ->
          case (enCodeInj ctx f g Refl, enCodeInj ctx a' b' Refl) of
            (Refl, Refl) -> Refl
    (EnCodedCon f, EnCodedCon g) ->
      case ctx of
        EmptyRep -> Refl
        c@(ctx' ::> k) ->
          case (f k c, g k c) of
            (Refl, Refl) -> enCodeInj ctx' a b Refl

--------------------------------------------------------------------------------
-- Decoding

type family DeCoder (ctx :: Ctx) (as :: Assign ctx) (u :: Code ctx uk) :: uk where
  DeCoder ctx as ('App f x) = (DeCoder ctx as f) (DeCoder ctx as x)
  DeCoder 'Empty 'AssignEmpty ('Const t) = t
  DeCoder (_ :> _) (_ :::> a) 'Var = a
  DeCoder (ctx :> _) (as :::> _) ('Weak u) = DeCoder ctx as u

type family DeCode0 (u :: Code 'Empty uk) :: uk where
  DeCode0 ('App f x) = (DeCode0 f) (DeCode0 x)
  DeCode0 ('Const t) = t

type family DeCode1 (u :: Code ('Empty :> k) uk) (a :: k) :: uk where
  DeCode1 ('App f x) a = (DeCode1 f a) (DeCode1 x a)
  DeCode1 'Var a = a
  DeCode1 ('Weak u) a = DeCode0 u

type family
  DeCode2
    (u :: Code ('Empty :> k :> l) uk)
    (a :: k)
    (b :: l)
    :: uk where
  DeCode2 ('App f x) a b = (DeCode2 f a b) (DeCode2 x a b)
  DeCode2 ('Weak u) a _ = DeCode1 u a
  DeCode2 'Var _ b = b

data DeCode1'
  (f :: uk -> Type)
  (u :: Code ('Empty :> k) uk)
  (a :: k)
  = DeCode1' (f (DeCode1 u a))

data DeCode2'
  (f :: uk -> Type)
  (u :: Code ('Empty :> k :> l) uk)
  (a :: k)
  (b :: l)
  = DeCode2' (f (DeCode2 u a b))

data Dec
  (f :: uk -> Type)
  (ctx :: Ctx)
  (u :: Code ctx uk)
  where
  Dec0 :: f (DeCode0 u) -> Dec f 'Empty u
  Dec1 :: All (DeCode1' f u) -> Dec f ('Empty :> ()) u

data All (f :: k) where
  All0 :: f -> All f
  AllS :: (forall a. All (f a)) -> All f

type family DeCode (ctx :: Ctx) (u :: Code ctx uk) (f :: uk -> Type) :: Type where
  DeCode 'Empty u f = f (DeCode0 u)
  DeCode ('Empty :> _) u f = All (DeCode1' f u)
  DeCode ('Empty :> _ :> _) u f = All (DeCode2' f u)

-- | Like 'Type.Reflection.TypeRep, but can create representatives of
-- polymorphic types.
data TypeRep (a :: k) where
  TypeRep :: CtxRep ctx -> CodeRep ctx k (EnCode ctx a) -> TypeRep a
  -- TypeRep :: CodeRep ctx k u -> TypeRep (DeCode ctx u Proxy)

-- | Helper for default signature for 'Typeable', not exported.
defTypeRep ::
  forall k (a :: k).
  Reflect.TypeRep a ->
  CodeRep 'Empty k (EnCode 'Empty a)
defTypeRep a =
  case enCoded a of
    EnCodedApp f x Refl -> AppRep (defTypeRep f) (defTypeRep x)
    EnCodedCon {} -> ConstRep a

class Reflect.Typeable k => Typeable (a :: k) where
  typeRep :: TypeRep a

  default typeRep :: Reflect.Typeable a => TypeRep a
  typeRep = TypeRep EmptyRep (defTypeRep (Reflect.typeRep @a))

-- instance TestEquality TypeRep where
--   testEquality (TypeRep ctx u) (TypeRep ctx' v) = do
--     Refl <- testEquality ctx ctx'
--     Refl <- testEquality (codeKind u) (codeKind v)
--     Refl <- testEquality u v
--     -- Refl <- return (enCodeInj ctx _ _ Refl)
--     return Refl

--------------------------------------------------------------------------------
-- Weakening

type family WeakBy (ctx :: Ctx) (u :: Code 'Empty k) :: Code ctx k where
  WeakBy 'Empty u = u
  WeakBy (ctx :> _) u = 'Weak (WeakBy ctx u)

weakBy :: CtxRep ctx -> CodeRep 'Empty k u -> CodeRep ctx k (WeakBy ctx u)
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
    (u :: Code (ctx :> k) uk)
    (a :: k)
    :: Code ctx uk where
  Inst ctx ('App f x) a = 'App (Inst ctx f a) (Inst ctx x a)
  Inst ctx 'Var a = WeakBy ctx ('Const a)
  Inst ctx ('Weak u) _ = u

inst :: Reflect.TypeRep a -> CodeRep (ctx :> k) uk u -> CodeRep ctx uk (Inst ctx u a)
inst rep =
  \case
    AppRep f a -> AppRep (inst rep f) (inst rep a)
    VarRep k ctx -> weakBy ctx (ConstRep rep)
    WeakRep _kRep u -> u

-- TODO(lb): ???
-- data TryInst (ctx :: Ctx) (u :: Code ctx) a where
--   InstFail :: TryInst Ctx0 u a
--   InstSucceed ::
--     CodeRep ctx (Inst ctx u a) -> TryInst (ctx :> ()) u a

-- tryInst :: Proxy a -> CodeRep ctx u -> TryInst ctx u a
-- tryInst proxy uRep =
--   case codeCtx uRep of
--     ERep -> InstFail
--     XRep ctx -> InstSucceed (inst proxy uRep)

--------------------------------------------------------------------------------
-- Application

-- -- TODO(lb)
-- appMono ::
--   DeCodereted 'Empty u ->
--   DeCode 'Empty u Identity ->
--   CodeRep 'Empty Type v ->
--   DeCode 'Empty v Identity ->

--------------------------------------------------------------------------------
-- Dynamic

-- | 'PolyFun' is the goal: A monomorphic type that captures the polymorphism of
-- the contained function.
data PolyFun where
  PolyFun :: CodeRep ctx Type u -> DeCode ctx u Identity -> PolyFun

--------------------------------------------------------------------------------
-- Examples

enCodedUnit :: CodeRep 'Empty Type (EnCode 'Empty ())
enCodedUnit = ConstRep (Reflect.typeRep @())

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

_var_1_1 :: CodeRep Ctx1 Type 'Var
_var_1_1 = VarRep kTypeRep ctx0

_var_2_1 :: CodeRep Ctx2 Type ('Weak 'Var)
_var_2_1 = WeakRep kTypeRep (VarRep kTypeRep ctx0)

_var_2_2 :: CodeRep Ctx2 Type 'Var
_var_2_2 = VarRep kTypeRep ctx1

type IdType1 = 'Var ---> 'Var

type ArrKind = Type -> Type -> Type

-- arrKindRep :: KindRep ArrKind
-- arrKindRep = kTypeRep :==> kTypeRep :==> kTypeRep

-- arrRep1 :: CodeRep Ctx1 ArrKind ('Weak ('Const (->)))
-- arrRep1 = WeakRep kTypeRep (ConstRep arrKindRep (Reflect.typeRep @(->)))

-- _idRep :: CodeRep Ctx1 Type IdType1
-- _idRep = AppRep (AppRep arrRep1 (VarRep kTypeRep ctx0)) (VarRep kTypeRep ctx0)

-- _idRepWeak :: CodeRep Ctx2 Type ('Weak IdType1)
-- _idRepWeak = WeakRep (VarRep rep0 :---> VarRep rep0)

-- _idRepWeak' :: CodeRep Ctx2 Type ('Weak 'Var ----> 'Weak 'Var)
-- _idRepWeak' = WeakRep var_1_1 :---> WeakRep var_1_1

-- _idInst :: CodeRep Ctx0 Type (Inst Ctx0 IdType1 Bool)
-- _idInst = WeakRep (ConstRep (Proxy @Bool)) :---> WeakRep (ConstRep (Proxy @Bool))

-- _idP :: PolyFun
-- _idP = PolyFun idRep (All1 (DeCode1' id))

-- constRep :: CodeRep Ctx2 Type ('Weak 'Var --> 'Var --> 'Weak 'Var)
-- constRep = var_2_1 :---> var_2_2 :---> var_2_1

-- _constP :: PolyFun
-- _constP = PolyFun constRep (All2 (DeCode2' const))

-- _getConst ::
--   All2 (DeCode2' ('Weak 'Var --> 'Var --> 'Weak 'Var)) ->
--   (forall a b. a -> b -> a)
-- _getConst (All2 (DeCode2' const_)) = const_
