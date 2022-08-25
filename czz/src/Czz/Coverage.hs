{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Czz.Coverage
  ( Coverage
  , empty
  , coverage
  , bucket
  , withCoverage
  )
where

import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (liftIO)
import           Data.Hashable (Hashable)
import qualified Data.Hashable as Hash
import           Data.IORef (IORef)
import qualified Data.IORef as IORef
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
import           Czz.Coverage.Bucket (Bucketing)
import qualified Czz.Coverage.Bucket as Bucket
import           Czz.Coverage.Path (Path)
import qualified Czz.Coverage.Path as Path
import           Czz.Freq (Freq)
import qualified Czz.Freq as Freq
import           Czz.Fuzz.Type (Fuzzer)
import qualified Czz.Fuzz.Type as Fuzz
import           Czz.KLimited (IsKLimited)
import qualified Czz.Log as Log
import qualified Czz.Record as Rec

newtype Coverage k = Coverage { getCoverage :: Freq (Path k) }
  deriving (Eq, Hashable, Ord, Show)

empty :: IsKLimited k => Coverage k
empty = Coverage Freq.empty

addPath :: Path k -> Coverage k -> Coverage k
addPath p = Coverage . Freq.inc p . getCoverage

bucket :: Bucketing -> Coverage k -> Coverage k
bucket b = Coverage . Freq.map (Bucket.bucket b) . getCoverage

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
  Bucketing ->
  Fuzzer ext env eff () ->
  Fuzzer ext env eff (Coverage k)
withCoverage b fuzzer =
  fuzzer
  { Fuzz.nextSeed = Fuzz.nextSeed fuzzer . fmap (fmap (const ()))
  , Fuzz.onUpdate = \state -> Fuzz.onUpdate fuzzer (() <$ state)
  , Fuzz.symbolicBits = \bak -> do
      bits <- Fuzz.symbolicBits fuzzer bak
      coverageRef <- IORef.newIORef empty
      feat <- coverage (Just (C.backendGetSym bak)) coverageRef
      return
        bits
        { Fuzz.getFeedback = do
            cover <- bucket b <$> IORef.readIORef coverageRef
            return (cover, Rec.FeedbackId (Hash.hash cover))
        , Fuzz.instrumentation = [feat]
        }
  }
