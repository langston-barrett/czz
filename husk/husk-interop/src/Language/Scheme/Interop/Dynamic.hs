{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Scheme.Interop.Dynamic
  ( dynAppPoly
  , dynAppPolyConst
  ) where

import           Data.Dynamic (Dynamic)
import qualified Data.Dynamic as Dyn
import           Data.Type.Equality ((:~~:)(HRefl))
import           Type.Reflection (Typeable)
import qualified Type.Reflection as Reflect

unwrap ::
  forall f b.
  Typeable f =>
  (forall a. Typeable a => f a -> b) ->
  Dynamic ->
  Maybe b
unwrap k =
  \case
    Dyn.Dynamic (Reflect.App fRep aRep) x -> do
      HRefl <- Reflect.eqTypeRep (Reflect.typeRep @f) fRep
      return (Reflect.withTypeable aRep (k x))
    _ -> Nothing

dynAppPoly ::
  forall f g.
  Typeable f =>
  Typeable g =>
  (forall a. Typeable a => f a -> g a) ->
  Dynamic ->
  Maybe Dynamic
dynAppPoly f = unwrap (Dyn.toDyn . f)

dynAppPolyConst ::
  forall f b.
  Typeable f =>
  Typeable b =>
  (forall a. f a -> b) ->
  Dynamic ->
  Maybe Dynamic
dynAppPolyConst f = unwrap (Dyn.toDyn . f)
