{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
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
  ) where

import           Data.Kind (Type)
import           Data.Proxy (Proxy(Proxy))

--------------------------------------------------------------------------------
-- Kinds

data Kind
  = KType
  | Kind :=> Kind

data KindRep (k :: Kind) where
  KTypeRep :: KindRep 'KType
  (:==>) :: KindRep k -> KindRep l -> KindRep (k ':=> l)

infixr :==>

-- TODO(lb): needs UndecidableInstances... safe? I think so?
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
    VarRep k ctxRep -> ctxRep ::> k
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

-- --------------------------------------------------------------------------------
-- -- Weakening

-- type family WeakBy (ctx :: Ctx) (u :: U Ctx0) :: U ctx where
--   WeakBy Ctx0 u = u
--   WeakBy (ctx :> ()) u = 'Weak (WeakBy ctx u)

-- weakBy :: CtxRep ctx -> URep Ctx0 u -> URep ctx (WeakBy ctx u)
-- weakBy ctxRep u =
--   case (ctxRep, u) of
--     (ERep, _) -> u
--     (XRep ctxRep', arr@(_ :---> _)) -> WeakRep (weakBy ctxRep' arr)
--     (XRep ctxRep', cst@(ConstRep {})) -> WeakRep (weakBy ctxRep' cst)

-- --------------------------------------------------------------------------------
-- -- Instantiation

-- type family Inst (ctx :: Ctx) (u :: U (ctx :> ())) (a :: Type) :: U ctx where
--   Inst ctx (u ':--> v) a = Inst ctx u a ':--> Inst ctx v a
--   Inst ctx ('Weak u) _ = u
--   Inst ctx 'Var a = WeakBy ctx ('Const a)

-- inst :: Proxy a -> URep (ctx :> ()) u -> URep ctx (Inst ctx u a)
-- inst proxy =
--   \case
--     u :---> v -> inst proxy u :---> inst proxy v
--     VarRep ctxRep -> weakBy ctxRep (ConstRep proxy)
--     WeakRep u -> u

-- -- TODO(lb): ???
-- -- data TryInst (ctx :: Ctx) (u :: U ctx) a where
-- --   InstFail :: TryInst Ctx0 u a
-- --   InstSucceed ::
-- --     URep ctx (Inst ctx u a) -> TryInst (ctx :> ()) u a

-- -- tryInst :: Proxy a -> URep ctx u -> TryInst ctx u a
-- -- tryInst proxy uRep =
-- --   case uCtx uRep of
-- --     ERep -> InstFail
-- --     XRep ctxRep -> InstSucceed (inst proxy uRep)

-- --------------------------------------------------------------------------------
-- -- Dynamic

-- -- | 'PolyFun' is the goal: A monomorphic type that captures the polymorphism of
-- -- the contained function.
-- data PolyFun where
--   PolyFun :: URep ctx u -> Interp ctx u -> PolyFun

-- --------------------------------------------------------------------------------
-- -- Examples

-- rep0 :: CtxRep Ctx0
-- rep0 = ERep

-- rep1 :: CtxRep Ctx1
-- rep1 = XRep ERep

-- var_1_1 :: URep Ctx1 'Var
-- var_1_1 = VarRep rep0

-- var_2_1 :: URep Ctx2 ('Weak 'Var)
-- var_2_1 = WeakRep (VarRep rep0)

-- var_2_2 :: URep Ctx2 'Var
-- var_2_2 = VarRep rep1

-- type IdType = 'Var ':--> 'Var

-- idRep :: URep Ctx1 IdType
-- idRep = var_1_1 :---> var_1_1

-- _idRepWeak :: URep ('ExtendCtx Ctx1) ('Weak IdType)
-- _idRepWeak = WeakRep (VarRep rep0 :---> VarRep rep0)

-- _idRepWeak' :: URep ('ExtendCtx Ctx1) ('Weak 'Var ':--> 'Weak 'Var)
-- _idRepWeak' = WeakRep var_1_1 :---> WeakRep var_1_1

-- _idInst :: URep Ctx1 (Inst Ctx1 IdType Bool)
-- _idInst = WeakRep (ConstRep (Proxy @Bool)) :---> WeakRep (ConstRep (Proxy @Bool))

-- _idP :: PolyFun
-- _idP = PolyFun idRep (All1 (Interp1' id))

-- constRep :: URep Ctx2 ('Weak 'Var ':--> 'Var ':--> 'Weak 'Var)
-- constRep = var_2_1 :---> var_2_2 :---> var_2_1

-- _constP :: PolyFun
-- _constP = PolyFun constRep (All2 (Interp2' const))

-- _getConst ::
--   All2 (Interp2' ('Weak 'Var ':--> 'Var ':--> 'Weak 'Var)) ->
--   (forall a b. a -> b -> a)
-- _getConst (All2 (Interp2' const_)) = const_
