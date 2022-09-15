{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
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
import           Data.Proxy (Proxy)

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

rep0 :: CtxRep Ctx0
rep0 = ERep

rep1 :: CtxRep Ctx1
rep1 = XRep ERep

rep2 :: CtxRep Ctx2
rep2 = XRep (XRep ERep)

-- | The universe of function types that mention variables in @ctx@
data U (ctx :: Ctx) where
  Arr :: U ctx -> U ctx -> U ctx
  Const :: Type -> U Ctx0
  Var :: U (ctx :> ())
  Weak :: U ctx -> U (ctx :> ())

type a --> b = 'Arr a b

data URep (ctx :: Ctx) (u :: U ctx) :: Type where
  ArrRep :: URep ctx u -> URep ctx v -> URep ctx (u --> v)
  ConstRep :: Proxy t -> URep Ctx0 ('Const t)
  VarRep :: CtxRep ctx -> URep (ctx :> ()) 'Var
  WeakRep :: URep ctx u -> URep (ctx :> ()) ('Weak u)

(-->) :: URep ctx u -> URep ctx v -> URep ctx (u --> v)
(-->) = ArrRep
infixr -->

type family Interp0 (u :: U Ctx0) :: Type where
  Interp0 (u --> v) = Interp0 u -> Interp0 v
  Interp0 ('Const t) = t

type family Interp1 (u :: U Ctx1) (a :: Type) :: Type where
  Interp1 (u --> v) a = Interp1 u a -> Interp1 v a
  Interp1 'Var a = a

type family Interp2 (u :: U Ctx2) (a :: Type) (b :: Type) :: Type where
  Interp2 (u --> v) a b = Interp2 u a b -> Interp2 v a b
  Interp2 ('Weak u) a b = Interp1 u a
  Interp2 'Var a b = b

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
--   Instantiate ctx (u --> v) l = Instantiate ctx u l -> Instantiate ctx v l
--   Instantiate 'EmptyCtx ('Const t) '[] = t
--   Instantiate (ctx :> ()) 'Var (x ': xs) = x
--   Instantiate (ctx :> ()) ('Weak u) (x ': xs) = Instantiate ctx u xs

-- -- For partial applications
-- newtype Inst ctx u l = Inst (Instantiate ctx u l)

-- data AllL u l where
--   AllLNil :: Instantiate 'EmptyCtx u '[] -> AllL u '[]
--   AllLCons :: (forall a. Instantiate (ctx :> ()) u (a ': l)) -> AllL u l

data PolyFun where
  PolyFun :: URep ctx u -> Interp ctx u -> PolyFun

--------------------------------------------------------------------------------
-- Examples

var_1_1 :: URep Ctx1 'Var
var_1_1 = VarRep rep0

var_2_1 :: URep Ctx2 ('Weak 'Var)
var_2_1 = WeakRep (VarRep rep0)

var_2_2 :: URep Ctx2 'Var
var_2_2 = VarRep rep1

idRep :: URep Ctx1 ('Var --> 'Var)
idRep = ArrRep var_1_1 var_1_1

idRepWeak :: URep ('ExtendCtx Ctx1) ('Weak ('Var --> 'Var))
idRepWeak = WeakRep (VarRep rep0 --> VarRep rep0)

idRepWeak' :: URep ('ExtendCtx Ctx1) ('Weak 'Var --> 'Weak 'Var)
idRepWeak' = WeakRep var_1_1 --> WeakRep var_1_1

idP :: PolyFun
idP = PolyFun idRep (All1 (Interp1' id))

constRep :: URep Ctx2 ('Weak 'Var --> 'Var --> 'Weak 'Var)
constRep = var_2_1 --> var_2_2 --> var_2_1

constP :: PolyFun
constP = PolyFun constRep (All2 (Interp2' const))

getConst ::
  All2 (Interp2' ('Weak 'Var --> 'Var --> 'Weak 'Var)) ->
  (forall a b. a -> b -> a)
getConst (All2 (Interp2' const_)) = const_
