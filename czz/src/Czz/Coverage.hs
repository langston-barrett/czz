{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Czz.Coverage
  ( Coverage
  , empty
  , coverage
  , bin
  , withCoverage
  )
where

import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (liftIO)
import           Data.Hashable (Hashable)
import qualified Data.Hashable as Hash
import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Nonce as Nonce

import qualified What4.ProgramLoc as What4

import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.FunctionHandle as C
import qualified Lang.Crucible.Simulator.CallFrame as C
import qualified Lang.Crucible.Simulator.EvalStmt as C
import qualified Lang.Crucible.Simulator.ExecutionTree as C

import qualified Czz.Coverage.BlockId as BlockId
import           Czz.Coverage.Path (Path)
import qualified Czz.Coverage.Path as Path
import           Czz.Fuzz.Type (Fuzzer)
import qualified Czz.Fuzz.Type as Fuzz
import           Czz.KLimited (IsKLimited)
import qualified Czz.Log as Log

newtype Coverage k = Coverage { getCoverage :: Map (Path k) Word }
  deriving (Eq, Hashable, Ord, Show)

empty :: IsKLimited k => Coverage k
empty = Coverage Map.empty

addPath :: Path k -> Coverage k -> Coverage k
addPath p = Coverage . Map.insertWith (+) p 1 . getCoverage

-- TODO(lb): AFL-style binning
bin :: Coverage k -> Coverage k
bin = id

-- TODO(lb): notion of maximum coverage? goal completed?
coverage ::
  Log.Has Text =>
  IsKLimited k =>
  proxy sym ->
  IORef (Coverage k) ->
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
          let msg = show (C.handleName handle) ++ " " ++ show loc
          liftIO $ Log.debug (Text.pack msg)
          let blkId = BlockId.new (C.handleName handle) blockIdInt
          path <- IORef.readIORef pathRef
          let path' = Path.snoc path blkId
          IORef.writeIORef pathRef path'
          IORef.modifyIORef coverRef (addPath path')
        _ -> return ()
      return C.ExecutionFeatureNoChange

-- | Turn a non-feedback-guided fuzzer into a /k/-edge-coverage guided one.
withCoverage ::
  IsKLimited k =>
  Fuzzer ext env eff () ->
  Fuzzer ext env eff (Coverage k)
withCoverage fuzzer =
  fuzzer
  { Fuzz.nextSeed = Fuzz.nextSeed fuzzer . fmap (fmap (const ()))
  , Fuzz.symbolicBits = \bak -> do
      bits <- Fuzz.symbolicBits fuzzer bak
      coverageRef <- IORef.newIORef empty
      feat <- coverage (Just (C.backendGetSym bak)) coverageRef
      return
        bits
        { Fuzz.getFeedback = do
            cover <- bin <$> IORef.readIORef coverageRef
            return (cover, Hash.hash cover)
        , Fuzz.instrumentation = [feat]
        }
  }
