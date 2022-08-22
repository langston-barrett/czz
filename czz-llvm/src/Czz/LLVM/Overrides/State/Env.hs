{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Czz.LLVM.Overrides.State.Env
  ( EnvState
  , mkEnvVar
  , getEnv
  )
where

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Maybe as Maybe
import qualified Data.Vector as Vec

import qualified What4.Interface as What4

-- crucible
import           Lang.Crucible.Backend (IsSymBackend, IsSymInterface)
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.CFG.Common as C
import qualified Lang.Crucible.FunctionHandle as C
import qualified Lang.Crucible.Simulator as C
import qualified Lang.Crucible.Types as C

type EnvState = C.VectorType (C.StringType C.Char8)

mkEnvVar ::
  C.HandleAllocator ->
  C.OverrideSim p sym ext rtp args ret (C.GlobalVar EnvState)
mkEnvVar halloc = do
  let repr = C.knownRepr :: C.TypeRepr EnvState
  var <- liftIO (C.freshGlobalVar halloc "czz:env" repr)
  C.writeGlobal var mempty
  return var

-- TODO(lb): initialize from envp

getEnv ::
  IsSymBackend sym bak =>
  bak ->
  C.GlobalVar EnvState ->
  ByteString ->
  C.OverrideSim p sym ext rtp args ret (Maybe ByteString)
getEnv bak envVar varName = do
  state <- C.readGlobal envVar
  let sym = C.backendGetSym bak
  return (Maybe.listToMaybe (Maybe.mapMaybe (isVar (Just sym) varName) (Vec.toList state)))
  where
    isVar ::
      IsSymInterface sym =>
      proxy sym ->
      ByteString ->
      What4.SymExpr sym (C.BaseStringType What4.Char8) ->
      Maybe ByteString
    isVar _proxy name strExpr =
      case What4.asString strExpr of
        Nothing -> Nothing
        Just (What4.Char8Literal str) ->
          let pfx = name <> "="
          in if pfx `BS.isPrefixOf` str
             then Just (BS.drop (BS.length pfx) str)
             else Nothing
