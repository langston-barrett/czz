{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

module Czz.KLimited
  ( KLimited
  , IsKLimited
  , withKLimit
  , empty
  , length
  , drop
  , snoc
  )
where

import           Prelude hiding (drop, length)
import           Data.Hashable (Hashable)
import           GHC.TypeLits (Nat)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import           Data.Parameterized.NatRepr (NatRepr)
import qualified Data.Parameterized.NatRepr as NatRepr
import           Data.Parameterized.Some (Some(Some))

newtype KLimited (k :: Nat) a
  = KLimited { getKLimited :: Seq a }
  deriving (Eq, Hashable, Ord, Show)
type role KLimited nominal representational

type IsKLimited k = ?kLimit :: NatRepr k

withKLimit :: Int -> (forall k. IsKLimited k => a) -> a
withKLimit k comp =
  case NatRepr.mkNatRepr (fromIntegral (max 0 k)) of
    Some kNat -> let ?kLimit = kNat in comp

empty :: IsKLimited k => KLimited k a
empty = KLimited Seq.empty

length :: KLimited k a -> Int
length = Seq.length . getKLimited

drop :: Int -> KLimited k a -> KLimited k a
drop i = KLimited . Seq.drop i . getKLimited

maxedOut :: IsKLimited k => KLimited k a -> Bool
maxedOut kl =
  let bound' = fromIntegral (NatRepr.natValue ?kLimit)
  in bound' /= 0 && length kl >= bound'

snoc :: IsKLimited k => KLimited k a -> a -> KLimited k a
snoc kl x =
  KLimited $ (Seq.|> x) $ getKLimited $ drop (if maxedOut kl then 1 else 0) kl
