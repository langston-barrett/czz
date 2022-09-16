{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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

module Language.Scheme.Interop.Poly where

import           Data.Kind (Type)
import           Data.Proxy (Proxy(Proxy))

--------------------------------------------------------------------------------
-- Contexts

-- | Context of unnamed type variables
data Ctx
  = EmptyCtx
  | ExtendCtx Ctx

-- | We think of contexts as being extended "on the right"
type ctx :> a = 'ExtendCtx ctx

type Ctx0 = 'EmptyCtx
type Ctx1 = 'EmptyCtx :> ()
type Ctx2 = 'EmptyCtx :> () :> ()

data CtxRep ctx where
  ERep :: CtxRep Ctx0
  XRep :: CtxRep ctx -> CtxRep (ctx :> ())

--------------------------------------------------------------------------------
-- Universe

-- | The universe of function types that mention variables in @ctx@
data U (ctx :: Ctx) where
  (:-->) :: U ctx -> U ctx -> U ctx
  Const :: Type -> U Ctx0
  Var :: U (ctx :> ())
  Weak :: U ctx -> U (ctx :> ())

infixr :-->

-- TODO(lb): type constructors
data URep (ctx :: Ctx) (u :: U ctx) :: Type where
  (:--->) :: URep ctx u -> URep ctx v -> URep ctx (u ':--> v)
  ConstRep :: Proxy t -> URep Ctx0 ('Const t)
  VarRep :: CtxRep ctx -> URep (ctx :> ()) 'Var
  WeakRep :: URep ctx u -> URep (ctx :> ()) ('Weak u)

infixr :--->

uCtx :: URep ctx u -> CtxRep ctx
uCtx =
  \case
    (u :---> _) -> uCtx u
    ConstRep {} -> ERep
    VarRep ctxRep -> XRep ctxRep
    WeakRep u -> XRep (uCtx u)

--------------------------------------------------------------------------------
-- Interpretation

type family Interp0 (u :: U Ctx0) :: Type where
  Interp0 (u ':--> v) = Interp0 u -> Interp0 v
  Interp0 ('Const t) = t

type family Interp1 (u :: U Ctx1) (a :: Type) :: Type where
  Interp1 (u ':--> v) a = Interp1 u a -> Interp1 v a
  Interp1 'Var a = a

type family Interp2 (u :: U Ctx2) (a :: Type) (b :: Type) :: Type where
  Interp2 (u ':--> v) a b = Interp2 u a b -> Interp2 v a b
  Interp2 ('Weak u) a _ = Interp1 u a
  Interp2 'Var _ b = b

newtype Interp1' u a = Interp1' (Interp1 u a)
newtype Interp2' u a b = Interp2' (Interp2 u a b)

data All1 f = All1 (forall a. f a)
data All2 f = All2 (forall a b. f a b)

type family Interp (ctx :: Ctx) (u :: U ctx) :: Type where
  Interp Ctx0 u = Interp0 u
  Interp Ctx1 u = All1 (Interp1' u)
  Interp Ctx2 u = All2 (Interp2' u)

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

type family WeakBy (ctx :: Ctx) (u :: U Ctx0) :: U ctx where
  WeakBy Ctx0 u = u
  WeakBy (ctx :> ()) u = 'Weak (WeakBy ctx u)

weakBy :: CtxRep ctx -> URep Ctx0 u -> URep ctx (WeakBy ctx u)
weakBy ctxRep u =
  case (ctxRep, u) of
    (ERep, _) -> u
    (XRep ctxRep', arr@(_ :---> _)) -> WeakRep (weakBy ctxRep' arr)
    (XRep ctxRep', cst@(ConstRep {})) -> WeakRep (weakBy ctxRep' cst)

--------------------------------------------------------------------------------
-- Instantiation

type family Inst (ctx :: Ctx) (u :: U (ctx :> ())) (a :: Type) :: U ctx where
  Inst ctx (u ':--> v) a = Inst ctx u a ':--> Inst ctx v a
  Inst ctx ('Weak u) _ = u
  Inst ctx 'Var a = WeakBy ctx ('Const a)

inst :: Proxy a -> URep (ctx :> ()) u -> URep ctx (Inst ctx u a)
inst proxy =
  \case
    u :---> v -> inst proxy u :---> inst proxy v
    VarRep ctxRep -> weakBy ctxRep (ConstRep proxy)
    WeakRep u -> u

-- TODO(lb): ???
-- data TryInst (ctx :: Ctx) (u :: U ctx) a where
--   InstFail :: TryInst Ctx0 u a
--   InstSucceed ::
--     URep ctx (Inst ctx u a) -> TryInst (ctx :> ()) u a

-- tryInst :: Proxy a -> URep ctx u -> TryInst ctx u a
-- tryInst proxy uRep =
--   case uCtx uRep of
--     ERep -> InstFail
--     XRep ctxRep -> InstSucceed (inst proxy uRep)

--------------------------------------------------------------------------------
-- Dynamic

-- | 'PolyFun' is the goal: A monomorphic type that captures the polymorphism of
-- the contained function.
data PolyFun where
  PolyFun :: URep ctx u -> Interp ctx u -> PolyFun

--------------------------------------------------------------------------------
-- Examples

rep0 :: CtxRep Ctx0
rep0 = ERep

rep1 :: CtxRep Ctx1
rep1 = XRep ERep

rep2 :: CtxRep Ctx2
rep2 = XRep (XRep ERep)

var_1_1 :: URep Ctx1 'Var
var_1_1 = VarRep rep0

var_2_1 :: URep Ctx2 ('Weak 'Var)
var_2_1 = WeakRep (VarRep rep0)

var_2_2 :: URep Ctx2 'Var
var_2_2 = VarRep rep1

type IdType = 'Var ':--> 'Var

idRep :: URep Ctx1 IdType
idRep = var_1_1 :---> var_1_1

idRepWeak :: URep ('ExtendCtx Ctx1) ('Weak IdType)
idRepWeak = WeakRep (VarRep rep0 :---> VarRep rep0)

idRepWeak' :: URep ('ExtendCtx Ctx1) ('Weak 'Var ':--> 'Weak 'Var)
idRepWeak' = WeakRep var_1_1 :---> WeakRep var_1_1

idInst :: URep Ctx1 (Inst Ctx1 IdType Bool)
idInst = WeakRep (ConstRep (Proxy @Bool)) :---> WeakRep (ConstRep (Proxy @Bool))

idP :: PolyFun
idP = PolyFun idRep (All1 (Interp1' id))

constRep :: URep Ctx2 ('Weak 'Var ':--> 'Var ':--> 'Weak 'Var)
constRep = var_2_1 :---> var_2_2 :---> var_2_1

constP :: PolyFun
constP = PolyFun constRep (All2 (Interp2' const))

getConst ::
  All2 (Interp2' ('Weak 'Var ':--> 'Var ':--> 'Weak 'Var)) ->
  (forall a b. a -> b -> a)
getConst (All2 (Interp2' const_)) = const_
