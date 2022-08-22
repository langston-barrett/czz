{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

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

-- crucible
import qualified Lang.Crucible.Simulator as C

-- crucible-llvm
import           Lang.Crucible.LLVM.Intrinsics (OverrideTemplate)

import qualified Czz.Log as Log
import           Czz.SysTrace (SomeSysTrace)

import qualified Czz.LLVM.Overrides.Libc as Libc
import qualified Czz.LLVM.Overrides.Posix as Posix
import qualified Czz.LLVM.Overrides.State.Env as State.Env
import           Czz.LLVM.Overrides.Util (OverrideConstraints)

data Effect
  = Libc !Libc.Effect
  | Posix !Posix.Effect
  deriving (Eq, Ord, Show)

_Libc :: Lens.Prism' Effect Libc.Effect
_Libc =
  Lens.prism'
    Libc
    (\case
      Libc eff -> Just eff
      _ -> Nothing)

_Posix :: Lens.Prism' Effect Posix.Effect
_Posix =
  Lens.prism'
    Posix
    (\case
      Posix eff -> Just eff
      _ -> Nothing)


overrides ::
  Log.Has Text =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (SomeSysTrace Effect) ->
  IORef [ByteString] ->
  C.GlobalVar State.Env.EnvState ->
  [OverrideTemplate p sym arch rtp l a]
overrides proxy effects envVarRef envVar =
  Log.adjust Text.pack $  -- TODO(lb): structured logging
    concat
      [ Libc.overrides proxy effects _Libc envVarRef envVar
      , Posix.overrides proxy effects _Posix envVar
      ]

-- TODO(lb): stdout, stderr global FILE*
-- TODO(lb): llvm.va_start
