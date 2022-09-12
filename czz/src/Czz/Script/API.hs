module Czz.Script.API
  ( extendEnv
  )
where

import           Control.Monad ((<=<))
import           Data.Text (Text)

import qualified Language.Scheme.Types as LST

import           Czz.Log (Logger, Msg)
import qualified Czz.Script.API.Config as APIConfig
import qualified Czz.Script.API.Fuzz as APIFuzz

extendEnv ::
  -- | stdout logger. TODO(lb): reify this in scripts.
  Logger (Msg Text) ->
  -- | stderr logger. TODO(lb): reify this in scripts.
  Logger (Msg Text) ->
  LST.Env ->
  IO LST.Env
extendEnv stdoutLogger stderrLogger =
  APIConfig.extendEnv "czz" <=<
    APIFuzz.extendEnv stdoutLogger stderrLogger "czz" 
