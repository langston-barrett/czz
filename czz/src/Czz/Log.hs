{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

module Czz.Log
  ( Logger
  , new
  , void
  , log
  , cmapM
  , Has
  , adjust
  )
where

import           Prelude hiding (log)

import           Data.Functor.Contravariant (Contravariant, (>$<))

import qualified Colog.Core.Action as Colog

-- | See 'Colog.LogAction'.
newtype Logger msg = Logger { getLogger :: Colog.LogAction IO msg }
  deriving ( Contravariant
           , Monoid
           , Semigroup
           )

new :: (msg -> IO ()) -> Logger msg
new = Logger . Colog.LogAction

void :: Logger a
void = Logger (Colog.LogAction (const (return ())))

log :: Logger msg -> msg -> IO ()
log logger = Colog.unLogAction (getLogger logger)

cmapM :: (a -> IO b) -> Logger b -> Logger a
cmapM f = Logger . Colog.cmapM f . getLogger

type Has msg = (?logger :: Logger msg)

adjust ::
  Has msg =>
  (msg' -> msg) ->
  (Has msg' => a) ->
  a
adjust f comp =
  let ?logger = f >$< ?logger
  in comp
