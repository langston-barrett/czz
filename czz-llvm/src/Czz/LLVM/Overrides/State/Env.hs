{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Czz.LLVM.Overrides.State.Env
  ( EnvState
  , mkEnvVar
  , getEnv
  , unsetEnv
  )
where

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Maybe as Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vec

import qualified What4.Interface as What4

-- crucible
import           Lang.Crucible.Backend (IsSymBackend, IsSymInterface)
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.CFG.Common as C
import qualified Lang.Crucible.FunctionHandle as C
import qualified Lang.Crucible.Simulator as C
import qualified Lang.Crucible.Types as C

import           Czz.LLVM.CString (CString)
import qualified Czz.LLVM.CString as CStr
import qualified Czz.Log as Log

type EnvState = C.VectorType (C.StringType C.Char8)

mkEnvVar ::
  IsSymInterface sym =>
  sym ->
  C.HandleAllocator ->
  Vec.Vector CString ->
  C.OverrideSim p sym ext rtp args ret (C.GlobalVar EnvState)
mkEnvVar sym halloc initVal = do
  let repr = C.knownRepr :: C.TypeRepr EnvState
  var <- liftIO (C.freshGlobalVar halloc "czz:env" repr)
  initSymVal <-
    liftIO (sequence (Vec.map (What4.stringLit sym . What4.Char8Literal . CStr.toByteString) initVal))
  C.writeGlobal var initSymVal
  return var

--------------------------------------------------------------------------------
-- ** getenv

getEnvIO ::
  Log.Has Text =>
  IsSymBackend sym bak =>
  bak ->
  C.RegValue sym EnvState ->
  ByteString ->
  IO (Maybe ByteString)
getEnvIO bak state varName = do
  let sym = C.backendGetSym bak
  logState (Just sym) state
  return (Maybe.listToMaybe (Maybe.mapMaybe (lookupIn (Just sym) varName) (Vec.toList state)))

getEnv ::
  Log.Has Text =>
  IsSymBackend sym bak =>
  bak ->
  C.GlobalVar EnvState ->
  ByteString ->
  C.OverrideSim p sym ext rtp args ret (Maybe ByteString)
getEnv bak envVar varName = do
  state <- C.readGlobal envVar
  liftIO (getEnvIO bak state varName)

--------------------------------------------------------------------------------
-- ** unsetenv

unsetEnvIO ::
  Log.Has Text =>
  IsSymBackend sym bak =>
  bak ->
  C.RegValue sym EnvState ->
  ByteString ->
  IO (Int, C.RegValue sym EnvState)
unsetEnvIO bak state varName = do
  let sym = C.backendGetSym bak
  liftIO (logState (Just sym) state)
  let final = Vec.filter (Maybe.isNothing . lookupIn (Just sym) varName) state
  return (0, final)

unsetEnv ::
  Log.Has Text =>
  IsSymBackend sym bak =>
  bak ->
  C.GlobalVar EnvState ->
  ByteString ->
  C.OverrideSim p sym ext rtp args ret Int
unsetEnv bak envVar varName =
  C.modifyGlobal envVar $ \state -> do
    liftIO (unsetEnvIO bak state varName)

--------------------------------------------------------------------------------
-- ** Helpers (Not Exported)

-- TODO(lb): pretty-print
logState ::
  Log.Has Text =>
  IsSymInterface sym =>
  proxy sym ->
  Vec.Vector (What4.SymExpr sym (C.BaseStringType What4.Char8)) ->
  IO ()
logState proxy state =
  Log.debug
    ("Env state: " <> Text.pack (show (Maybe.mapMaybe (toString proxy) (Vec.toList state))))

toString ::
  IsSymInterface sym =>
  proxy sym ->
  What4.SymExpr sym (C.BaseStringType What4.Char8) ->
  Maybe ByteString
toString _proxy strExpr =
  case What4.asString strExpr of
    Nothing -> Nothing
    Just (What4.Char8Literal str) -> Just (str :: ByteString)

lookupIn ::
  IsSymInterface sym =>
  proxy sym ->
  ByteString ->
  What4.SymExpr sym (C.BaseStringType What4.Char8) ->
  Maybe ByteString
lookupIn proxy name strExpr =
  case toString proxy strExpr of
    Nothing -> Nothing
    Just str ->
      let pfx = name <> "="
      in if pfx `BS.isPrefixOf` str
          then Just (BS.drop (BS.length pfx) str)
          else Nothing
