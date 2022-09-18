{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
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
    -- * Environments
  , type Env(..)
  , EnvRep(..)
    -- * Universe
  , type U(..)
  , URep(..)
  , uCtx
  , uKind
  -- * Encoding
  , Encode
  , encodeMonoInj
  -- * Decoding
  , Decode
  -- * Weakening
  , WeakBy
  , weakBy
  -- * Instantiation
  , inst
  ) where

import           Data.Kind (Type)
import           Data.Proxy (Proxy(Proxy))
import           Data.Type.Equality (TestEquality(testEquality), (:~:)(Refl))
import qualified Type.Reflection as R
import           Unsafe.Coerce (unsafeCoerce)

unsafeEqualityAxiom :: a :~: b
unsafeEqualityAxiom = unsafeCoerce Refl

--------------------------------------------------------------------------------
-- Monotypeable

type Monotypeable = R.Typeable
type MonotypeRep = R.TypeRep

--------------------------------------------------------------------------------
-- Kinds

-- These are just to make it clear when we're talking about kinds (types of
-- types), rather than just types.

-- | As we all know, since @-XTypeInType@ kinds /are/ types.
type Kind = Type

newtype KindRep k = KindRep { _getKindRep :: MonotypeRep k }
  deriving (TestEquality)

kindRep :: forall a. Monotypeable a => KindRep a
kindRep = KindRep (R.typeRep @a)

typeRepKind :: forall k (a :: k). MonotypeRep a -> KindRep k
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

--------------------------------------------------------------------------------
-- Environments

-- | A list of types with kinds specified by @ctx@
data Env (ctx :: Ctx) where
  EEmpty :: Env 'Empty
  (:&) ::
    forall ctx k.
    Env ctx ->
    k ->
    Env (ctx ':> k)

data EnvRep (env :: Env ctx) where
  EEmptyRep :: EnvRep 'EEmpty
  (::&) ::
    EnvRep env ->
    MonotypeRep (a :: k) ->
    EnvRep (env ':& a)

assignCtx ::
  forall (ctx :: Ctx) (env :: Env ctx).
  EnvRep env ->
  CtxRep ctx
assignCtx =
  \case
    EEmptyRep -> EmptyRep
    r ::& t -> assignCtx r ::> typeRepKind t

-- instance TestEquality EnvRep where
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

data V (ctx :: Ctx) (uk :: Kind) where
  Base :: U ctx uk -> V ctx uk
  Forall :: KindRep k -> U (ctx ':> k) uk -> V ctx uk

data URep (ctx :: Ctx) (uk :: Kind) (u :: U ctx uk) :: Type where
  AppRep :: URep ctx (k -> l) f -> URep ctx k u -> URep ctx l ('App f u)
  ConstRep :: MonotypeRep c -> URep 'Empty uk ('Const c)
  VarRep :: KindRep uk -> CtxRep ctx -> URep (ctx ':> uk) uk 'Var
  WeakRep :: KindRep k -> URep ctx uk u -> URep (ctx ':> k) uk ('Weak u)

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
    ConstRep t -> typeRepKind t
    VarRep k _ -> k
    WeakRep _ u -> uKind u

instance TestEquality (URep ctx u) where
  testEquality u v =
    case (u, v) of
      (AppRep ku lu, AppRep kv lv) -> do
        Refl <- testEquality (uKind ku) (uKind kv)
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
    MonotypeRep f ->
    MonotypeRep b ->
    (forall proxy ctx. proxy ctx -> Encode ctx a :~: 'App (Encode ctx f) (Encode ctx b)) ->
    Encoded a
  EncodedCon ::
    (Encode 'Empty a ~ 'Const a) =>
    (forall proxy proxy' k ctx.
     proxy k ->
     proxy' ctx ->
     Encode (ctx ':> k) a :~: 'Weak (Encode ctx a)) ->
    Encoded a

encoded :: forall a. MonotypeRep a -> Encoded a
encoded =
  \case
    R.Con _ ->
      case unsafeEqualityAxiom :: Encode 'Empty a :~: 'Const a of
        Refl -> EncodedCon (\_k _c -> (unsafeEqualityAxiom :: Encode (ctx ':> k) a :~: 'Weak (Encode ctx a)))
    R.App f x -> EncodedApp f x (const Refl)
    R.Fun {} -> error "Impossible"

appInj1 :: 'App f a :~: 'App g b -> f :~: g
appInj1 Refl = Refl

appInj2 :: 'App f a :~: 'App g b -> a :~: b
appInj2 Refl = Refl

encodeMonoInjAppOuter ::
  forall f a g b ctx.
  CtxRep ctx ->
  MonotypeRep f ->
  MonotypeRep a ->
  MonotypeRep g ->
  MonotypeRep b ->
  Encode ctx (f a) :~: Encode ctx (g b) ->
  f :~: g
encodeMonoInjAppOuter ctx f a g b Refl = encodeMonoInj ctx f g (appInj1 r)
  where
    r :: 'App (Encode ctx g) (Encode ctx b) :~: 'App (Encode ctx f) (Encode ctx a)
    r = Refl

encodeMonoInjAppInner ::
  forall f a g b ctx.
  CtxRep ctx ->
  MonotypeRep f ->
  MonotypeRep a ->
  MonotypeRep g ->
  MonotypeRep b ->
  Encode ctx (f a) :~: Encode ctx (g b) ->
  a :~: b
encodeMonoInjAppInner ctx f a g b Refl = encodeMonoInj ctx a b (appInj2 r)
  where
    r :: 'App (Encode ctx g) (Encode ctx b) :~: 'App (Encode ctx f) (Encode ctx a)
    r = Refl

encodeMonoInj ::
  forall a b ctx.
  CtxRep ctx ->
  MonotypeRep a ->
  MonotypeRep b ->
  Encode ctx a :~: Encode ctx b ->
  a :~: b
encodeMonoInj ctx a b r@Refl =
  case (encoded a, encoded b) of
    (EncodedApp f a' _p1, EncodedApp g b' _p2) ->
      case ( encodeMonoInjAppOuter ctx f a' g b' r
           , encodeMonoInjAppInner ctx f a' g b' r
           ) of
        (Refl, Refl) ->
          case (encodeMonoInj ctx f g Refl, encodeMonoInj ctx a' b' Refl) of
            (Refl, Refl) -> Refl
    (EncodedCon f, EncodedCon g) ->
      case ctx of
        EmptyRep -> Refl
        ctx' ::> k ->
          case (f k ctx', g k ctx') of
            (Refl, Refl) -> encodeMonoInj ctx' a b Refl
    _ -> error "Impossible"
    -- TODO(lb): when uncommenting these proofs, the above cases start failing to
    -- typecheck...?
    --
    -- (EncodedApp f a' p, EncodedCon g) ->
    --   case (p ctx, ctx) of
    --     (Refl, ctx' ::> k) ->
    --       case g @Maybe @Maybe (Just k) (Just ctx') of {}
    -- (EncodedCon g, EncodedApp f a' p) ->
    --   case (p ctx, ctx) of
    --     (Refl, ctx' ::> k) ->
    --       case g @Maybe @Maybe (Just k) (Just ctx') of {}

encodePolyInj ::
  forall k a b ctx.
  CtxRep ctx ->
  URep ctx k (Encode ctx a) ->
  URep ctx k (Encode ctx b) ->
  Encode ctx a :~: Encode ctx b ->
  a :~: b
encodePolyInj _ctx _u _v Refl = unsafeEqualityAxiom

--------------------------------------------------------------------------------
-- Decoding

type family Decode (ctx :: Ctx) (env :: Env ctx) (u :: U ctx uk) :: uk where
  Decode ctx env ('App f x) = (Decode ctx env f) (Decode ctx env x)
  Decode 'Empty 'EEmpty ('Const t) = t
  Decode (_ ':> _) (_ ':& a) 'Var = a
  Decode (ctx ':> _) (env ':& _) ('Weak u) = Decode ctx env u

-- | Like 'Type.Reflection.TypeRep, but can create representatives of
-- polymorphic types.
data PolytypeRep (a :: k) where
  PolytypeRep ::
    Proxy a ->
    CtxRep ctx ->
    URep ctx k (Encode ctx a) ->
    PolytypeRep a
  -- PolytypeRep :: CtxRep ctx -> URep ctx k u -> PolytypeRep (Decode ctx as u)

-- | Helper for default signature for 'Typeable', not exported.
defPolytypeRep ::
  forall k (a :: k).
  MonotypeRep a ->
  URep 'Empty k (Encode 'Empty a)
defPolytypeRep a =
  case encoded a of
    EncodedApp f x _ -> AppRep (defPolytypeRep f) (defPolytypeRep x)
    EncodedCon {} -> ConstRep a

class Monotypeable k => Typeable (a :: k) where
  typeRep :: PolytypeRep a

  default typeRep :: Monotypeable a => PolytypeRep a
  typeRep = PolytypeRep (Proxy @a) EmptyRep (defPolytypeRep (R.typeRep @a))

instance TestEquality PolytypeRep where
  testEquality
    (PolytypeRep (Proxy :: Proxy a) ctx (u :: URep ctx k (Encode ctx a)))
    (PolytypeRep (Proxy :: Proxy b) ctx' v) = do

    Refl <- testEquality ctx ctx'
    Refl <- testEquality (uKind u) (uKind v)
    Refl <- testEquality u v
    Refl <- return (encodePolyInj @k @a @b ctx u v Refl)
    return Refl

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

inst :: MonotypeRep a -> URep (ctx ':> k) uk u -> URep ctx uk (Inst ctx u a)
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
--   case uCtx uRep of
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
