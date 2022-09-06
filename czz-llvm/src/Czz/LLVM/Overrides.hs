{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Czz.LLVM.Overrides
  ( Effect(..)
  , overrides
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as AesonTH
import qualified Control.Lens as Lens
import           Data.ByteString (ByteString)
import           Data.IORef (IORef)
import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Lang.Crucible.Simulator as C

-- crucible-llvm
import           Lang.Crucible.LLVM.Intrinsics (OverrideTemplate)

import           Czz.Overrides (EffectTrace)
import qualified Czz.Log as Log

import qualified Czz.LLVM.Overrides.Env as Env
import qualified Czz.LLVM.Overrides.Libc as Libc
import qualified Czz.LLVM.Overrides.Hostname as Hostname
import qualified Czz.LLVM.Overrides.Printf as Printf
import qualified Czz.LLVM.Overrides.Socket as Socket
import qualified Czz.LLVM.Overrides.Signal as Signal
import           Czz.LLVM.Overrides.State.Env as State.Env
import qualified Czz.LLVM.Overrides.Time as Time
import           Czz.LLVM.Overrides.Util (OverrideConstraints)

data Effect
  = Env !Env.Effect
  | Libc !Libc.Effect
  | Hostname !Hostname.Effect
  | Printf !Printf.Effect
  | Socket !Socket.Effect
  | Signal !Signal.Effect
  | Time !Time.Effect
  deriving (Eq, Ord, Show)

$(Lens.makePrisms ''Effect)
$(AesonTH.deriveJSON Aeson.defaultOptions ''Effect)

overrides ::
  -- TODO(lb): structured logging
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace Effect) ->
  IORef [ByteString] ->
  C.GlobalVar State.Env.EnvState ->
  [OverrideTemplate p sym arch rtp l a]
overrides proxy effects envVarRef envStateVar =
    concat
      [ Env.overrides proxy effects _Env envVarRef envStateVar
      , Hostname.overrides proxy effects _Hostname
      , Log.adjust Text.pack $  -- TODO(lb): Text
          Libc.overrides proxy effects _Libc envVarRef
      , Printf.overrides proxy effects _Printf
      , Signal.overrides proxy effects _Signal
      , Log.adjust Text.pack $  -- TODO(lb): Text
          Socket.overrides proxy effects _Socket
      , Time.overrides proxy effects _Time
      ]
