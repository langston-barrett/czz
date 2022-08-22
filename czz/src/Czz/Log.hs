{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Czz.Log
  ( Logger
  , bare
  , void
  , log
  , cmapM
  , adjust
  -- * Severity
  , CologS.Severity(..)
  , WithSeverity
  -- * Msg
  , Msg
  , Has
  , with
  , new
  , debug
  , error
  , info
  , warn
  , toDebug
  , toError
  , toInfo
  , toWarn
  )
where

import           Prelude hiding (error, log)

import           Data.Functor.Contravariant (Contravariant, (>$<))
import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Colog.Core.Action as Colog
import           Colog.Core.Severity (Severity)
import qualified Colog.Core.Severity as CologS

-- | See 'Colog.LogAction'.
newtype Logger msg = Logger { getLogger :: Colog.LogAction IO msg }
  deriving ( Contravariant
           , Monoid
           , Semigroup
           )

bare :: (a -> IO ()) -> Logger a
bare = Logger . Colog.LogAction

void :: Logger a
void = Logger (Colog.LogAction (const (return ())))

log :: Logger msg -> msg -> IO ()
log logger = Colog.unLogAction (getLogger logger)

cmapM :: (a -> IO b) -> Logger b -> Logger a
cmapM f = Logger . Colog.cmapM f . getLogger

adjust ::
  Has msg =>
  (msg' -> msg) ->
  (Has msg' => a) ->
  a
adjust f comp =
  let ?logger = fmap f >$< ?logger
  in comp

data WithSeverity a
  = WithSeverity
    { severity :: CologS.Severity
    , _val :: a
    }
  deriving Functor

withSeverity :: WithSeverity Text -> Text
withSeverity (WithSeverity s v) =
  "[" <> Text.toLower (Text.pack (show s)) <> "] " <> v

above :: Severity -> WithSeverity a -> Bool
above s = (s <=) . severity

newtype Msg a = Msg { getMsg :: WithSeverity a }
  deriving Functor

type Has msg = (?logger :: Logger (Msg msg))

with :: Logger (Msg msg) -> (Has msg => a) -> a
with logger comp = let ?logger = logger in comp

new :: Severity -> (Text -> IO ()) -> Logger (Msg Text)
new s =
  Logger . (getMsg >$<) . Colog.cfilter (above s) . (withSeverity >$<) . Colog.LogAction

debug :: Has msg => msg -> IO ()
debug = log ?logger . toDebug

error :: Has msg => msg -> IO ()
error = log ?logger . toError

info :: Has msg => msg -> IO ()
info = log ?logger . toInfo

warn :: Has msg => msg -> IO ()
warn = log ?logger . toWarn

toDebug :: Has msg => msg -> Msg msg
toDebug = Msg . WithSeverity CologS.Debug

toError :: Has msg => msg -> Msg msg
toError = Msg . WithSeverity CologS.Error

toInfo :: Has msg => msg -> Msg msg
toInfo = Msg . WithSeverity CologS.Info

toWarn :: Has msg => msg -> Msg msg
toWarn = Msg . WithSeverity CologS.Warning
