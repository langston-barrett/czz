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
    Kind
  , KindRep
  , kindRep
  , typeRepKind
  , kTypeRep
  , kDomain
  , kRange
    -- * Contexts
  , type Ctx(..)
  , CtxRep(..)
    -- * Environments
  , type Env(..)
  , EnvRep(..)
  , envCtx
    -- * Universe
  , type U(..)
  , URep(..)
  , uCtx
  , uKind
  -- * Encoding
  , Encode
  , encodeMonoInj
  -- * Decoding
  , Decode0
  , Decode
  , DecodeV(..)
  , DecodeV0
  -- * PolytypeRep
  , PolytypeRep(..)
  , Polytypeable
  , polytypeRep
  -- * Dynamic
  , Dynamic
  , toDyn
  , fromDyn
  -- * Weakening
  , WeakBy
  , weakBy
  -- * Instantiation
  , Inst
  , inst
  , TryInst(..)
  , tryInst
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

kDomain :: KindRep (k -> l) -> KindRep k
kDomain =
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

-- | Context of (mono)type variables
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

-- | A list of (mono)types with kinds specified by @ctx@
data Env (ctx :: Ctx) where
  EEmpty :: Env 'Empty
  (:&) ::
    forall ctx k.
    Env ctx ->
    k ->
    Env (ctx ':> k)

data EnvRep (ctx :: Ctx) (env :: Env ctx) where
  EEmptyRep :: EnvRep 'Empty 'EEmpty
  (::&) ::
    EnvRep ctx env ->
    MonotypeRep (a :: k) ->
    EnvRep (ctx ':> k) (env ':& a)

envCtx ::
  forall (ctx :: Ctx) (env :: Env ctx).
  EnvRep ctx env ->
  CtxRep ctx
envCtx =
  \case
    EEmptyRep -> EmptyRep
    r ::& t -> envCtx r ::> typeRepKind t

instance TestEquality (EnvRep ctx) where
  testEquality x y =
    case (x, y) of
      (EEmptyRep, EEmptyRep) -> Just Refl
      (x' ::& t, y' ::& u) -> do
        Refl <- testEquality x' y'
        Refl <- testEquality t u
        return Refl

--------------------------------------------------------------------------------
-- Universe

-- | Descriptions of the universe of types of kind @uk@ that mention variables
-- in @ctx@.
--
-- Other possible names: @Desc@, @Code@.
data U (ctx :: Ctx) (uk :: Kind) where
  -- TODO(lb): Try adding this for non-prenex polymorphism:
  All :: k -> U (ctx ':> k) (k -> uk) -> U ctx uk
  App :: U ctx (k -> uk) -> U ctx k -> U ctx uk
  Const :: k -> U 'Empty k
  Var :: U (ctx ':> uk) uk
  Weak :: U ctx uk -> U (ctx ':> k) uk

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

instance TestEquality (URep ctx uk) where
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

-- V is currently not used...

data V (ctx :: Ctx) (uk :: Kind) where
  Base :: U ctx uk -> V ctx uk
  Forall :: Kind -> V (ctx ':> k) uk -> V ctx uk

data VRep (ctx :: Ctx) (uk :: Kind) (v :: V ctx uk) where
  BaseRep :: URep ctx uk u -> VRep ctx uk ('Base u)
  ForallRep :: KindRep k -> VRep (ctx ':> k) uk v -> VRep ctx uk ('Forall k v)

instance TestEquality (VRep ctx uk) where
  testEquality x y =
    case (x, y) of
      (BaseRep x', BaseRep y') -> do
        Refl <- testEquality x' y'
        return Refl
      (ForallRep k x', ForallRep l y') -> do
        Refl <- testEquality k l
        Refl <- testEquality x' y'
        return Refl
      (_, _) -> Nothing


--------------------------------------------------------------------------------
-- Encoding

-- Needs UndecidableInstances
type family Encode (ctx :: Ctx) (a :: uk) = (u :: U ctx uk) where
  Encode ctx (f a) = 'App (Encode ctx f) (Encode ctx a)
  -- NB: With -XImpredicativeTypes, this case also catches polytypes.
  Encode 'Empty a = 'Const a
  Encode (ctx ':> _) a = 'Weak (Encode ctx a)

-- Later, we'll need proofs that Encode is injective. The following proofs use
-- a hopefully-unobjectionable axiom to show this for the case of monotypes, and
-- then postulate it for the case of polytypes. Alternatively, we can just add
-- an injectivity annotation to 'Encode' and use 'UndecidableInstances'... but
-- I'm not sure how safe that is. Are we just at risk of type-checking
-- non-termination?
--
-- TODO(lb): ask in Mattermost Haskell channel about this

data Encoded a where
  EncodedApp ::
    (a ~ f b) =>
    MonotypeRep f ->
    MonotypeRep b ->
    (forall proxy ctx. proxy ctx -> Encode ctx a :~: 'App (Encode ctx f) (Encode ctx b)) ->
    Encoded a
  EncodedCon ::
    ( Encode 'Empty a ~ 'Const a
    ) =>
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
    -- Not sure why GHC can't see this, since we have {-# COMPLETE App, Con #-}
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
encodeMonoInjAppOuter ctx f _a g _b Refl = encodeMonoInj ctx f g (appInj1 r)
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
encodeMonoInjAppInner ctx _f a _g b Refl = encodeMonoInj ctx a b (appInj2 r)
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
unsafeEncodePolyInj ::
  forall k a b ctx.
  CtxRep ctx ->
  URep ctx k (Encode ctx a) ->
  URep ctx k (Encode ctx b) ->
  Encode ctx a :~: Encode ctx b ->
  a :~: b
unsafeEncodePolyInj _ctx _u _v Refl = unsafeEqualityAxiom

--------------------------------------------------------------------------------
-- Decoding

-- | Called @elU0@ in van Noort and Sweirstra
type family Decode0 (u :: U 'Empty uk) :: uk where
  Decode0 ('App f x) = (Decode0 f) (Decode0 x)
  Decode0 ('Const t) = t

-- | Called @elU@ in van Noort and Sweirstra
type family Decode (ctx :: Ctx) (env :: Env ctx) (u :: U ctx uk) :: uk where
  Decode 'Empty 'EEmpty a = Decode0 a
  Decode ctx env ('App f x) = (Decode ctx env f) (Decode ctx env x)
  Decode (_ ':> _) (_ ':& a) 'Var = a
  Decode (ctx ':> _) (env ':& _) ('Weak u) = Decode ctx env u

-- type family DecodeV (ctx :: Ctx) (env :: Env ctx) (v :: V ctx Type) :: Type where
--   DecodeV ctx env ('Base u) = Decode ctx env u
--   -- • Illegal polymorphic type: forall (a :: k). DecodeV (ctx ':> k) (env ':& a) v
--   DecodeV ctx env ('Forall k v) = forall (a :: k). DecodeV (ctx :> k) (env :& a) v

-- | Called @elV@ in van Noort and Sweirstra
data DecodeV (ctx :: Ctx) (env :: Env ctx) (v :: V ctx Type) where
  DecodeBase :: Decode ctx env u -> DecodeV ctx env ('Base u)
  DecodeForall ::
    (forall (a :: k). DecodeV (ctx ':> k) (env ':& a) v) ->
    DecodeV ctx env ('Forall k v)

type family DecodeV0 (v :: V 'Empty Type) where
  DecodeV0 ('Base u) = Decode0 u
  DecodeV0 ('Forall k v) = DecodeV 'Empty 'EEmpty ('Forall k v)

--------------------------------------------------------------------------------
-- PolytypeRepU

-- Problem with this approach: when writing
--
-- idRep :: PolytypeRep (forall a. a -> a)
--
-- get
--
-- • Couldn't match type: 'App ('App ('Weak ('Const (->))) 'Var) 'Var
--                  with: 'Weak ('Const (forall a. a -> a))
--
-- and type families can't mention quantification, so this can't be fixed.

-- | Like 'Type.Reflection.TypeRep, but can create representatives of
-- polymorphic types.
data PolytypeRepU (a :: k) where
  PolytypeRepU ::
    Proxy a ->
    CtxRep ctx ->
    URep ctx k (Encode ctx a) ->
    PolytypeRepU a

defPolytypeRepU ::
  forall k (a :: k).
  MonotypeRep a ->
  URep 'Empty k (Encode 'Empty a)
defPolytypeRepU a =
  case encoded a of
    EncodedApp f x _ -> AppRep (defPolytypeRepU f) (defPolytypeRepU x)
    EncodedCon {} -> ConstRep a

instance TestEquality PolytypeRepU where
  testEquality
    (PolytypeRepU (Proxy :: Proxy a) ctx (u :: URep ctx k (Encode ctx a)))
    (PolytypeRepU (Proxy :: Proxy b) ctx' v) = do

    Refl <- testEquality ctx ctx'
    Refl <- testEquality (uKind u) (uKind v)
    Refl <- testEquality u v
    Refl <- return (unsafeEncodePolyInj @k @a @b ctx u v Refl)
    return Refl

--------------------------------------------------------------------------------
-- PolytypeRep

-- | Like 'Type.Reflection.TypeRep, but can create representatives of
-- polymorphic types.
data PolytypeRep (a :: k) where
  -- PolytypeRep ::
  --   Proxy a ->
  --   CtxRep ctx ->
  --   URep ctx k (Encode ctx a) ->
  --   PolytypeRep a
  --
  -- This makes PolytypeRep non-poly-kinded, since DecodeV0 requires kind Type.
  --
  PolytypeRep ::
    VRep 'Empty Type v ->
    PolytypeRep (DecodeV0 v)

data IsBase v where
  IsBase :: IsBase ('Base ('Const u))

decodeV0App ::
  forall v f b.
  (DecodeV0 v ~ f b) =>
  v :~: 'Base ('App (Encode 'Empty f) (Encode 'Empty b))
decodeV0App = undefined

decodeV0Con ::
  forall v a.
  (DecodeV0 v ~ a) =>
  MonotypeRep a ->
  IsBase v
decodeV0Con = undefined

defPolytypeRep ::
  forall v.
  MonotypeRep (DecodeV0 v) ->
  VRep 'Empty Type v
defPolytypeRep a =
  case encoded a of
    EncodedApp (f :: MonotypeRep f) (b :: MonotypeRep b) _ ->
      case decodeV0App @v @f @b of
        Refl -> BaseRep (AppRep (defPolytypeRepU f) (defPolytypeRepU b))
    EncodedCon {} ->
      case decodeV0Con @v a of
        IsBase -> BaseRep (ConstRep a)

instance TestEquality PolytypeRep where
  testEquality
    (PolytypeRep (Proxy :: Proxy a) ctx (u :: URep ctx k (Encode ctx a)))
    (PolytypeRep (Proxy :: Proxy b) ctx' v) = do

    Refl <- testEquality ctx ctx'
    Refl <- testEquality (uKind u) (uKind v)
    Refl <- testEquality u v
    Refl <- return (unsafeEncodePolyInj @k @a @b ctx u v Refl)
    return Refl

--------------------------------------------------------------------------------
-- Dynamic

data Dynamic where
  Dynamic :: PolytypeRep a -> a -> Dynamic

toDyn :: PolytypeRep a -> a -> Dynamic
toDyn a = Dynamic a

fromDyn :: PolytypeRep a -> Dynamic -> Maybe a
fromDyn rep (Dynamic rep' a) =
  case testEquality rep rep' of
    Just Refl -> Just a
    Nothing -> Nothing

--------------------------------------------------------------------------------
-- Weakening

type family WeakBy (ctx :: Ctx) (u :: U 'Empty uk) :: U ctx uk where
  WeakBy 'Empty u = u
  WeakBy (ctx ':> _) u = 'Weak (WeakBy ctx u)

weakBy :: CtxRep ctx -> URep 'Empty uk u -> URep ctx uk (WeakBy ctx u)
weakBy ctx u =
  case (ctx, u) of
    (EmptyRep, _) -> u
    (ctx' ::> k, a@(AppRep {})) -> WeakRep k (weakBy ctx' a)
    (ctx' ::> k, c@(ConstRep {})) -> WeakRep k (weakBy ctx' c)

--------------------------------------------------------------------------------
-- Instantiation

type family
  Inst
    (a :: k)
    (ctx :: Ctx)
    (u :: U (ctx ':> k) uk)
    :: U ctx uk where
  Inst a ctx ('App f x) = 'App (Inst a ctx f) (Inst a ctx x)
  Inst a ctx 'Var = WeakBy ctx ('Const a)
  Inst _ _ ('Weak u) = u

inst ::
  MonotypeRep a ->
  URep (ctx ':> k) uk u ->
  URep ctx uk (Inst a ctx u)
inst a =
  \case
    AppRep f x -> AppRep (inst a f) (inst a x)
    VarRep _k ctx -> weakBy ctx (ConstRep a)
    WeakRep _k u -> u

data TryInst (a :: k) (ctx :: Ctx) (u :: U ctx uk)  where
  InstFailVar :: TryInst a 'Empty u
  InstFailKind :: TryInst a ctx u
  InstSucceed :: URep ctx uk (Inst a ctx u) -> TryInst a (ctx ':> k) u

tryInst :: MonotypeRep a -> URep ctx uk u -> TryInst a ctx u
tryInst a u =
  case uCtx u of
    EmptyRep -> InstFailVar
    _ctx ::> k ->
      case testEquality k (typeRepKind a) of
        Nothing -> InstFailKind
        Just Refl -> InstSucceed (inst a u)

--------------------------------------------------------------------------------
-- Substitutions

-- TODO(lb): See Swierstra
