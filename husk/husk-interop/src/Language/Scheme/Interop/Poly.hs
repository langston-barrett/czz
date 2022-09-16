{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
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
  -- * Interpretation
  , Interp
  , Interp1'(..)
  , Interp2'(..)
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
import           Data.Proxy (Proxy(Proxy))

--------------------------------------------------------------------------------
-- Kinds

data Kind
  = KType
  | Kind :=> Kind

infixr :=>

data KindRep (k :: Kind) where
  KTypeRep :: KindRep 'KType
  (:==>) :: KindRep k -> KindRep l -> KindRep (k ':=> l)

infixr :==>

-- Needs UndecidableInstances... I believe this is safe, since given any RHS you
-- can deduce the RHS by looking at arrows and parens...
type family InterpKind (k :: Kind) = (t :: Type) | t -> k where
  InterpKind 'KType = Type
  InterpKind (k ':=> l) = InterpKind k -> InterpKind l

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

--------------------------------------------------------------------------------
-- Universe

-- | The universe of types that mention variables in @ctx@
data U (ctx :: Ctx) (uk :: Kind) where
  App :: U ctx (k ':=> uk) -> U ctx k -> U ctx uk
  Const :: InterpKind k -> U 'Empty k
  Var :: U (ctx :> uk) uk
  Weak :: U ctx uk -> U (ctx :> k) uk

data URep (ctx :: Ctx) (k :: Kind) (u :: U ctx uk) :: Type where
  AppRep :: URep ctx (k ':=> l) f -> URep ctx k u -> URep ctx l ('App f u)
  ConstRep :: KindRep uk -> Proxy f -> URep 'Empty uk ('Const f)
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
    AppRep f _a ->
      case uKind f of
        _k :==> l -> l
    ConstRep k _ -> k
    VarRep k _ -> k
    WeakRep _ u -> uKind u

--------------------------------------------------------------------------------
-- Interpretation

-- TODO(lb): con
type family Interp0 (u :: U 'Empty uk) :: InterpKind uk where
  Interp0 ('Const t) = t

type family Interp1 (u :: U ('Empty :> k) uk) (a :: InterpKind k) :: InterpKind uk where
  Interp1 'Var a = a
  Interp1 ('Weak u) a = Interp0 u

type family
  Interp2
    (u :: U ('Empty :> k :> l) uk)
    (a :: InterpKind k)
    (b :: InterpKind l)
    :: InterpKind uk where
  Interp2 ('Weak u) a _ = Interp1 u a
  Interp2 'Var _ b = b

data Interp1'
  (f :: InterpKind uk -> Type)
  (u :: U ('Empty :> k) uk)
  (a :: InterpKind k)
  = Interp1' (f (Interp1 u a))

data Interp2'
  (f :: InterpKind uk -> Type)
  (u :: U ('Empty :> k :> l) uk)
  (a :: InterpKind k)
  (b :: InterpKind l)
  = Interp2' (f (Interp2 u a b))

data All1 (f :: k -> Type) = All1 (forall (a :: k). f a)
data All2 (f :: k -> l -> Type) = All2 (forall (a :: k) (b :: l). f a b)

type family Interp (ctx :: Ctx) (u :: U ctx uk) (f :: InterpKind uk -> Type) :: Type where
  Interp 'Empty u f = f (Interp0 u)
  Interp ('Empty :> _) u f = All1 (Interp1' f u)
  Interp ('Empty :> _ :> _) u f = All2 (Interp2' f u)

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
    (a :: InterpKind k)
    :: U ctx uk where
  Inst ctx ('App f x) a = 'App (Inst ctx f a) (Inst ctx x a)
  Inst ctx 'Var a = WeakBy ctx ('Const a)
  Inst ctx ('Weak u) _ = u

inst :: Proxy a -> URep (ctx :> k) uk u -> URep ctx uk (Inst ctx u a)
inst proxy =
  \case
    AppRep f a -> AppRep (inst proxy f) (inst proxy a)
    VarRep k ctx -> weakBy ctx (ConstRep k proxy)
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
-- Dynamic

-- | 'PolyFun' is the goal: A monomorphic type that captures the polymorphism of
-- the contained function.
data PolyFun where
  PolyFun :: URep ctx 'KType u -> Interp ctx u Identity -> PolyFun

--------------------------------------------------------------------------------
-- Examples

type Ctx0 = 'Empty
type Ctx1 = 'Empty :> 'KType
type Ctx2 = 'Empty :> 'KType :> 'KType
-- type a --> b = 'App ('App ('Const (->)) a) b
type a ---> b = 'App ('App ('Weak ('Const (->))) a) b
-- type a ----> b = 'App ('App ('Weak ('Weak ('Const (->)))) a) b

ctx0 :: CtxRep Ctx0
ctx0 = EmptyRep

ctx1 :: CtxRep Ctx1
ctx1 = ctx0 ::> KTypeRep

_var_1_1 :: URep Ctx1 'KType 'Var
_var_1_1 = VarRep KTypeRep ctx0

_var_2_1 :: URep Ctx2 'KType ('Weak 'Var)
_var_2_1 = WeakRep KTypeRep (VarRep KTypeRep ctx0)

_var_2_2 :: URep Ctx2 'KType 'Var
_var_2_2 = VarRep KTypeRep ctx1

type IdType1 = 'Var ---> 'Var

type ArrKind = 'KType ':=> 'KType ':=> 'KType

arrKindRep :: KindRep ArrKind
arrKindRep = KTypeRep :==> KTypeRep :==> KTypeRep

arrRep1 :: URep Ctx1 ArrKind ('Weak ('Const (->)))
arrRep1 = WeakRep KTypeRep (ConstRep arrKindRep (Proxy @(->)))

_idRep :: URep Ctx1 'KType IdType1
_idRep = AppRep (AppRep arrRep1 (VarRep KTypeRep ctx0)) (VarRep KTypeRep ctx0)

-- _idRepWeak :: URep Ctx2 'KType ('Weak IdType1)
-- _idRepWeak = WeakRep (VarRep rep0 :---> VarRep rep0)

-- _idRepWeak' :: URep Ctx2 'KType ('Weak 'Var ----> 'Weak 'Var)
-- _idRepWeak' = WeakRep var_1_1 :---> WeakRep var_1_1

-- _idInst :: URep Ctx0 'KType (Inst Ctx0 IdType1 Bool)
-- _idInst = WeakRep (ConstRep (Proxy @Bool)) :---> WeakRep (ConstRep (Proxy @Bool))

-- _idP :: PolyFun
-- _idP = PolyFun idRep (All1 (Interp1' id))

-- constRep :: URep Ctx2 'KType ('Weak 'Var --> 'Var --> 'Weak 'Var)
-- constRep = var_2_1 :---> var_2_2 :---> var_2_1

-- _constP :: PolyFun
-- _constP = PolyFun constRep (All2 (Interp2' const))

-- _getConst ::
--   All2 (Interp2' ('Weak 'Var --> 'Var --> 'Weak 'Var)) ->
--   (forall a b. a -> b -> a)
-- _getConst (All2 (Interp2' const_)) = const_
