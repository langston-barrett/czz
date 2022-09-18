{-# LANGUAGE ConstraintKinds #-}
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
  , type U(..)
  , URep(..)
  , codeCtx
  , codeKind
  -- * Encoding
  , Encode
  , encodeInj
  -- * Decoding
  , Decode
  -- * Weakening
  , WeakBy
  , weakBy
  -- * Instantiation
  , inst
  ) where

import           Data.Kind (Type)
import           Data.Type.Equality (TestEquality(testEquality), (:~:)(Refl))
import qualified Type.Reflection as R
import           Unsafe.Coerce (unsafeCoerce)

type Monotypeable = R.Typeable

--------------------------------------------------------------------------------
-- Kinds

-- These are just to make it clear when we're talking about kinds (types of
-- types), rather than just types.

-- | As we all know, since @-XTypeInType@ kinds /are/ types.
type Kind = Type

newtype KindRep k = KindRep { _getKindRep :: R.TypeRep k }
  deriving (TestEquality)

kindRep :: forall a. Monotypeable a => KindRep a
kindRep = KindRep (R.typeRep @a)

typeRepKind :: forall k (a :: k). R.TypeRep a -> KindRep k
typeRepKind a = KindRep (R.typeRepKind a)

kTypeRep :: KindRep Type
kTypeRep = KindRep (R.typeRep @Type)

_kDomain :: KindRep (k -> l) -> KindRep k
_kDomain =
  -- The pattern matches are redundant, but GHC doesn't know that since they're
  -- pattern synonyms.
  \case
    KindRep (R.App (R.App (R.Con _) k) _) -> KindRep k
    KindRep (R.Fun k _) -> KindRep k

kRange :: KindRep (k -> l) -> KindRep l
kRange =
  \case
    KindRep (R.App (R.App (R.Con _) _) l) -> KindRep l
    KindRep (R.Fun _ l) -> KindRep l

--------------------------------------------------------------------------------
-- Contexts

-- | Context of type variables
data Ctx
  = Empty
  -- | We think of contexts as being extended "on the right"
  | Ctx :> Kind

data CtxRep ctx where
  EmptyRep :: CtxRep 'Empty
  (::>) :: CtxRep ctx -> KindRep k -> CtxRep (ctx ':> k)

instance TestEquality CtxRep where
  testEquality c d =
    case (c, d) of
      (EmptyRep, EmptyRep) -> Just Refl
      (ctx ::> k, ctx' ::> k') -> do
        Refl <- testEquality ctx ctx'
        Refl <- testEquality k k'
        return Refl
      (_, _) -> Nothing

-- | A list of types with kinds specified by @ctx@
data Assign (ctx :: Ctx) where
  AnEmpty :: Assign 'Empty
  (:&) ::
    forall ctx k a.
    Assign ctx ->
    k ->
    Assign (ctx ':> k)

data AssignRep (as :: Assign ctx) where
  AnEmptyRep :: AssignRep 'AnEmpty
  (::&) ::
    AssignRep as ->
    R.TypeRep (a :: k) ->
    AssignRep (as ':& a)

assignCtx ::
  forall (ctx :: Ctx) (as :: Assign ctx).
  AssignRep as ->
  CtxRep ctx
assignCtx =
  \case
    AnEmptyRep -> EmptyRep
    r ::& t -> assignCtx r ::> typeRepKind t

-- instance TestEquality AssignRep where
--   testEquality x y =
--     case (x, y) of
--       (AnEmptyRep, AnEmptyRep) -> Just Refl
--       (x' ::& t, y' ::& u) -> do
--         Refl <- testEquality (typeRepKind t) (typeRepKind u)
--         Refl <- testEquality t u
--         Refl <- testEquality x' y'
--         -- Refl <- testEquality (assignCtx x) (assignCtx y)
--         return Refl

--------------------------------------------------------------------------------
-- Universe

-- | Descriptions of the universe of types of kind @uk@ that mention variables
-- in @ctx@.
--
-- Other possible names: @Desc@, @Code@.
data U (ctx :: Ctx) (uk :: Kind) where
  -- TODO(lb): Try adding this for non-prenex polymorphism:
  -- All :: k -> U (ctx ':> k) -> U ctx
  App :: U ctx (k -> uk) -> U ctx k -> U ctx uk
  Const :: k -> U 'Empty k
  Var :: U (ctx ':> uk) uk
  Weak :: U ctx uk -> U (ctx ':> k) uk

data URep (ctx :: Ctx) (uk :: Kind) (u :: U ctx uk) :: Type where
  AppRep :: URep ctx (k -> l) f -> URep ctx k u -> URep ctx l ('App f u)
  ConstRep :: R.TypeRep c -> URep 'Empty uk ('Const c)
  VarRep :: KindRep uk -> CtxRep ctx -> URep (ctx ':> uk) uk 'Var
  WeakRep :: KindRep k -> URep ctx uk u -> URep (ctx ':> k) uk ('Weak u)

codeCtx :: URep ctx k u -> CtxRep ctx
codeCtx =
  \case
    AppRep f _ -> codeCtx f
    ConstRep {} -> EmptyRep
    VarRep k ctx -> ctx ::> k
    WeakRep k u -> codeCtx u ::> k

codeKind :: URep ctx k u -> KindRep k
codeKind =
  \case
    AppRep f _a -> kRange (codeKind f)
    ConstRep t -> typeRepKind t
    VarRep k _ -> k
    WeakRep _ u -> codeKind u

instance TestEquality (URep ctx u) where
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

type family Encode (ctx :: Ctx) (a :: k) = (u :: U ctx k) where
  Encode ctx (f a) = 'App (Encode ctx f) (Encode ctx a)
  Encode 'Empty a = 'Const a
  Encode (ctx ':> _) a = 'Weak (Encode ctx a)

data Encoded a where
  EncodedApp ::
    (a ~ f b) =>
    R.TypeRep f ->
    R.TypeRep b ->
    (forall ctx. Encode ctx a :~: 'App (Encode ctx f) (Encode ctx b)) ->
    Encoded a
  EncodedCon ::
    (Encode 'Empty a ~ 'Const a) =>
    (forall proxy proxy' k ctx.
     proxy k ->
     proxy' ctx ->
     Encode (ctx ':> k) a :~: 'Weak (Encode ctx a)) ->
    Encoded a

encoded :: forall a. R.TypeRep a -> Encoded a
encoded =
  \case
    R.Con _ ->
      -- TODO(lb): Justify!
      case unsafeCoerce Refl :: Encode 'Empty a :~: 'Const a of
        Refl -> EncodedCon (\_k _c -> (unsafeCoerce Refl :: Encode (ctx ':> k) a :~: 'Weak (Encode ctx a)))
    R.App f x -> EncodedApp f x Refl
    R.Fun {} -> error "Impossible"

appInj1 :: 'App f a :~: 'App g b -> f :~: g
appInj1 Refl = Refl

appInj2 :: 'App f a :~: 'App g b -> a :~: b
appInj2 Refl = Refl

encodeInjAppOuter ::
  forall f a g b ctx.
  CtxRep ctx ->
  R.TypeRep f ->
  R.TypeRep a ->
  R.TypeRep g ->
  R.TypeRep b ->
  Encode ctx (f a) :~: Encode ctx (g b) ->
  f :~: g
encodeInjAppOuter ctx f a g b Refl = encodeInj ctx f g (appInj1 r)
  where
    r :: 'App (Encode ctx g) (Encode ctx b) :~: 'App (Encode ctx f) (Encode ctx a)
    r = Refl

encodeInjAppInner ::
  forall f a g b ctx.
  CtxRep ctx ->
  R.TypeRep f ->
  R.TypeRep a ->
  R.TypeRep g ->
  R.TypeRep b ->
  Encode ctx (f a) :~: Encode ctx (g b) ->
  a :~: b
encodeInjAppInner ctx f a g b Refl = encodeInj ctx a b (appInj2 r)
  where
    r :: 'App (Encode ctx g) (Encode ctx b) :~: 'App (Encode ctx f) (Encode ctx a)
    r = Refl

encodeInj ::
  forall a b ctx.
  CtxRep ctx ->
  R.TypeRep a ->
  R.TypeRep b ->
  Encode ctx a :~: Encode ctx b ->
  a :~: b
encodeInj ctx a b r@Refl =
  case (encoded a, encoded b) of
    (EncodedApp f a' Refl, EncodedApp g b' Refl) ->
      case (encodeInjAppOuter ctx f a' g b' r, encodeInjAppInner ctx f a' g b' r) of
        (Refl, Refl) ->
          case (encodeInj ctx f g Refl, encodeInj ctx a' b' Refl) of
            (Refl, Refl) -> Refl
    (EncodedCon f, EncodedCon g) ->
      case ctx of
        EmptyRep -> Refl
        ctx' ::> k ->
          case (f k ctx', g k ctx') of
            (Refl, Refl) -> encodeInj ctx' a b Refl
    -- (EncodedApp f a' Refl, EncodedCon g) -> _

--------------------------------------------------------------------------------
-- Decoding

type family Decode (ctx :: Ctx) (as :: Assign ctx) (u :: U ctx uk) :: uk where
  Decode ctx as ('App f x) = (Decode ctx as f) (Decode ctx as x)
  Decode 'Empty 'AnEmpty ('Const t) = t
  Decode (_ ':> _) (_ ':& a) 'Var = a
  Decode (ctx ':> _) (as ':& _) ('Weak u) = Decode ctx as u

-- | Like 'Type.Rion.TypeRep, but can create representatives of
-- polymorphic types.
data TypeRep (a :: k) where
  TypeRep :: CtxRep ctx -> URep ctx k (Encode ctx a) -> TypeRep a
  -- TypeRep :: URep ctx k u -> TypeRep (DeU ctx u Proxy)

-- | Helper for default signature for 'Typeable', not exported.
defTypeRep ::
  forall k (a :: k).
  R.TypeRep a ->
  URep 'Empty k (Encode 'Empty a)
defTypeRep a =
  case encoded a of
    EncodedApp f x Refl -> AppRep (defTypeRep f) (defTypeRep x)
    EncodedCon {} -> ConstRep a

class Monotypeable k => Typeable (a :: k) where
  typeRep :: TypeRep a

  default typeRep :: Monotypeable a => TypeRep a
  typeRep = TypeRep EmptyRep (defTypeRep (R.typeRep @a))

-- instance TestEquality TypeRep where
--   testEquality (TypeRep ctx u) (TypeRep ctx' v) = do
--     Refl <- testEquality ctx ctx'
--     Refl <- testEquality (codeKind u) (codeKind v)
--     Refl <- testEquality u v
--     -- Refl <- return (encodeInj ctx _ _ Refl)
--     return Refl

--------------------------------------------------------------------------------
-- Weakening

type family WeakBy (ctx :: Ctx) (u :: U 'Empty k) :: U ctx k where
  WeakBy 'Empty u = u
  WeakBy (ctx ':> _) u = 'Weak (WeakBy ctx u)

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
    (u :: U (ctx ':> k) uk)
    (a :: k)
    :: U ctx uk where
  Inst ctx ('App f x) a = 'App (Inst ctx f a) (Inst ctx x a)
  Inst ctx 'Var a = WeakBy ctx ('Const a)
  Inst ctx ('Weak u) _ = u

inst :: R.TypeRep a -> URep (ctx ':> k) uk u -> URep ctx uk (Inst ctx u a)
inst rep =
  \case
    AppRep f a -> AppRep (inst rep f) (inst rep a)
    VarRep k ctx -> weakBy ctx (ConstRep rep)
    WeakRep _kRep u -> u

-- TODO(lb): ???
-- data TryInst (ctx :: Ctx) (u :: U ctx) a where
--   InstFail :: TryInst Ctx0 u a
--   InstSucceed ::
--     URep ctx (Inst ctx u a) -> TryInst (ctx :> ()) u a

-- tryInst :: Proxy a -> URep ctx u -> TryInst ctx u a
-- tryInst proxy uRep =
--   case codeCtx uRep of
--     ERep -> InstFail
--     XRep ctx -> InstSucceed (inst proxy uRep)

--------------------------------------------------------------------------------
-- Substitutions

-- TODO(lb): See Swierstra

--------------------------------------------------------------------------------
-- Examples

encodedUnit :: URep 'Empty Type (Encode 'Empty ())
encodedUnit = ConstRep (R.typeRep @())

type Ctx0 = 'Empty
type Ctx1 = 'Empty ':> Type
type Ctx2 = 'Empty ':> Type ':> Type
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
-- arrRep1 = WeakRep kTypeRep (ConstRep arrKindRep (R.typeRep @(->)))

-- _idRep :: URep Ctx1 Type IdType1
-- _idRep = AppRep (AppRep arrRep1 (VarRep kTypeRep ctx0)) (VarRep kTypeRep ctx0)

-- _idRepWeak :: URep Ctx2 Type ('Weak IdType1)
-- _idRepWeak = WeakRep (VarRep rep0 :---> VarRep rep0)

-- _idRepWeak' :: URep Ctx2 Type ('Weak 'Var ----> 'Weak 'Var)
-- _idRepWeak' = WeakRep var_1_1 :---> WeakRep var_1_1

-- _idInst :: URep Ctx0 Type (Inst Ctx0 IdType1 Bool)
-- _idInst = WeakRep (ConstRep (Proxy @Bool)) :---> WeakRep (ConstRep (Proxy @Bool))

-- _idP :: PolyFun
-- _idP = PolyFun idRep (All1 (DeU1' id))

-- constRep :: URep Ctx2 Type ('Weak 'Var --> 'Var --> 'Weak 'Var)
-- constRep = var_2_1 :---> var_2_2 :---> var_2_1

-- _constP :: PolyFun
-- _constP = PolyFun constRep (All2 (DeU2' const))

-- _getConst ::
--   All2 (DeU2' ('Weak 'Var --> 'Var --> 'Weak 'Var)) ->
--   (forall a b. a -> b -> a)
-- _getConst (All2 (DeU2' const_)) = const_
