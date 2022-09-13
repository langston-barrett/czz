{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Czz.Script.API.Fuzz
  ( extendEnv
  , SFuzzer(..)
  , SState(..)
  , fuzz
  ) where

import           Control.Monad.Except (ExceptT(..))  -- for auto
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import           Data.Functor ((<&>))
import           Data.Text (Text)

import           Data.Parameterized.Nonce (Nonce)
import qualified Data.Parameterized.Nonce as Nonce

import           Lang.Crucible.CFG.Extension (IsSyntaxExtension)

import qualified Language.Scheme.Types as LST

import           Language.Scheme.CustFunc (CustFunc)
import qualified Language.Scheme.CustFunc as Cust
import           Language.Scheme.Opaque (Opaque(..))  -- for auto

import           Czz.Log (Logger, Msg)
import qualified Czz.Config.Type as Conf
import           Czz.KLimited (IsKLimited)
import           Czz.Fuzz.Type (Fuzzer)
import           Czz.Fuzz (FuzzError)
import qualified Czz.Fuzz as Fuzz
import           Czz.State (State)
import qualified Czz.Stop as Stop

extendEnv ::
  -- | stdout logger. TODO(lb): reify this in scripts.
  Logger (Msg Text) ->
  -- | stderr logger. TODO(lb): reify this in scripts.
  Logger (Msg Text) ->
  String ->
  LST.Env ->
  IO LST.Env
extendEnv stdoutLogger stderrLogger pfx e = do
  Cust.extendEnv funcs pfx e
  where
    funcs =
      [ fuzz stdoutLogger stderrLogger
      ]

-- | Helper, not exported
_lift :: IO a -> LST.IOThrowsError a
_lift = liftIO
{-# INLINE _lift #-}

-- | Helper, not exported
_lift1 :: (a -> IO b) -> a -> LST.IOThrowsError b
_lift1 f a = liftIO (f a)
{-# INLINE _lift1 #-}

-- | Helper, not exported
lift2 :: (a -> b -> IO c) -> a -> b -> LST.IOThrowsError c
lift2 f a b = liftIO (f a b)
{-# INLINE lift2 #-}

-- | Monomorphic view of 'Fuzzer' exposed to Scheme.
data SFuzzer where
  SFuzzer ::
    ( Aeson.ToJSON env
    , Aeson.ToJSON eff
    , Aeson.ToJSON fb
    , IsKLimited k
    , IsSyntaxExtension ext
    ) =>
    Nonce Nonce.GlobalNonceGenerator ext ->
    Nonce Nonce.GlobalNonceGenerator env ->
    Nonce Nonce.GlobalNonceGenerator eff ->
    Nonce Nonce.GlobalNonceGenerator fb ->
    Fuzzer ext env eff k fb ->
    SFuzzer

-- | Monomorphic view of 'State' exposed to Scheme.
data SState where
  SState ::
    ( Aeson.ToJSON env
    , Aeson.ToJSON eff
    , Aeson.ToJSON fb
    , IsKLimited k
    , IsSyntaxExtension ext
    ) =>
    Nonce Nonce.GlobalNonceGenerator ext ->
    Nonce Nonce.GlobalNonceGenerator env ->
    Nonce Nonce.GlobalNonceGenerator eff ->
    Nonce Nonce.GlobalNonceGenerator fb ->
    State env eff k fb ->
    SState

fuzz ::
  Logger (Msg Text) ->
  Logger (Msg Text) ->
  CustFunc
fuzz stdoutLogger stderrLogger =
  Cust.CustFunc
  { Cust.custFuncName = "fuzz"
  , Cust.custFuncImpl = Cust.evalHuskable (Cust.auto (lift2 impl))
  }
  where
    impl ::
      SFuzzer ->
      Conf.FuzzConfig ->
      IO (Either FuzzError SState)
    impl (SFuzzer next nenv neff nfb fuzzer) fuzzConf = do
      stop <- Stop.new
      liftIO (Fuzz.fuzz fuzzConf stop fuzzer stdoutLogger stderrLogger) <&>
        \case
          Left err -> Left err
          Right state -> Right (SState next nenv neff nfb state)
