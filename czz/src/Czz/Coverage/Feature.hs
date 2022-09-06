{-# LANGUAGE OverloadedStrings #-}

module Czz.Coverage.Feature
  ( coverage
  )
where

import qualified Control.Lens as Lens
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Nonce as Nonce

import qualified What4.ProgramLoc as What4

import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.FunctionHandle as C
import qualified Lang.Crucible.Simulator.CallFrame as C
import qualified Lang.Crucible.Simulator.EvalStmt as C
import qualified Lang.Crucible.Simulator.ExecutionTree as C

import qualified Czz.Coverage.BlockId as BlockId
import qualified Czz.Coverage.Path as Path
import           Czz.Coverage.Seed (SeedCoverage)
import qualified Czz.Coverage.Seed as CSeed
import           Czz.KLimited (IsKLimited)
import qualified Czz.Log as Log

-- TODO(lb): notion of maximum coverage? goal completed?
coverage ::
  Log.Has Text =>
  IsKLimited k =>
  proxy sym ->
  IORef (SeedCoverage k) ->
  IO (C.GenericExecutionFeature sym)
coverage _proxy coverRef = do
  pathRef <- IORef.newIORef Path.empty
  return $
    C.GenericExecutionFeature $ \execState -> do
      case execState of
        C.RunningState (C.RunBlockStart cBlockId) simState -> do
          let blockIdInt = Ctx.indexVal (C.blockIDIndex cBlockId)
          C.CallFrame cfg _ _ _ _ _ <-
            return (simState Lens.^. C.stateCrucibleFrame)
          let handle = C.cfgHandle cfg
          let _handleId = Nonce.indexValue (C.handleID handle)
          let loc = simState Lens.^. C.stateLocation . Lens.to (fmap What4.plSourceLoc)
          let txtLoc = Text.pack (maybe "<unknown>" show loc)
          let msg = Text.pack (show (C.handleName handle)) <> " " <> txtLoc
          liftIO $ Log.debug msg
          let blkId = BlockId.new (C.handleName handle) blockIdInt
          path <- IORef.readIORef pathRef
          let path' = Path.snoc path blkId
          IORef.writeIORef pathRef path'
          IORef.modifyIORef coverRef (CSeed.addPath path')
          unless (loc == Just What4.InternalPos) $
            IORef.modifyIORef coverRef (CSeed.setLastLoc txtLoc)
        _ -> return ()
      return C.ExecutionFeatureNoChange
