{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Czz.LLVM.Overrides
  ( Effect(..)
  , overrides
  )
where

import qualified Control.Lens as Lens
import           Data.ByteString (ByteString)
import           Data.IORef (IORef)
import           Data.Text (Text)
import qualified Data.Text as Text

-- crucible-llvm
import           Lang.Crucible.LLVM.Intrinsics (OverrideTemplate)

import           Czz.Overrides (EffectTrace)
import qualified Czz.Log as Log

import qualified Czz.LLVM.Overrides.Libc as Libc
import qualified Czz.LLVM.Overrides.Hostname as Hostname
import qualified Czz.LLVM.Overrides.Socket as Socket
import           Czz.LLVM.Overrides.Util (OverrideConstraints)

data Effect
  = Libc !Libc.Effect
  | Hostname !Hostname.Effect
  | Socket !Socket.Effect
  deriving (Eq, Ord, Show)

$(Lens.makePrisms ''Effect)

overrides ::
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace Effect) ->
  IORef [ByteString] ->
  [OverrideTemplate p sym arch rtp l a]
overrides proxy effects envVarRef =
    concat
      [ Log.adjust Text.pack $  -- TODO(lb): structured logging
          Libc.overrides proxy effects _Libc envVarRef
      , Log.adjust Text.pack $  -- TODO(lb): structured logging
          Socket.overrides proxy effects _Socket
      ]
