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
import qualified Czz.LLVM.Overrides.Printf as Printf
import qualified Czz.LLVM.Overrides.Socket as Socket
import           Czz.LLVM.Overrides.Util (OverrideConstraints)

data Effect
  = Libc !Libc.Effect
  | Hostname !Hostname.Effect
  | Printf !Printf.Effect
  | Socket !Socket.Effect
  deriving (Eq, Ord, Show)

$(Lens.makePrisms ''Effect)

overrides ::
  -- TODO(lb): structured logging
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace Effect) ->
  IORef [ByteString] ->
  [OverrideTemplate p sym arch rtp l a]
overrides proxy effects envVarRef =
    concat
      [ Hostname.overrides proxy effects _Hostname
      , Log.adjust Text.pack $  -- TODO(lb): Text
          Libc.overrides proxy effects _Libc envVarRef
      , Printf.overrides proxy effects _Printf
      , Log.adjust Text.pack $  -- TODO(lb): Text
          Socket.overrides proxy effects _Socket
      ]
