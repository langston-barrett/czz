{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Czz.Coverage
  ( Coverage
  , empty
  , coverage
  , bucket
  , lastLoc
  , withCoverage
  )
where

import qualified Control.Lens as Lens
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Hashable (Hashable)
import qualified Data.Hashable as Hash
import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Generics (Generic)

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

data Coverage k =
  Coverage
  { covFreq :: !(Freq (Path k))
  , covLastLoc :: !Text
  }
  deriving (Eq, Generic, Ord, Show)

instance Hashable (Coverage k) where

empty :: IsKLimited k => Coverage k
empty = Coverage Freq.empty "<start>"

onPaths :: (Freq (Path k) -> Freq (Path k)) -> Coverage k -> Coverage k
onPaths f c = c { covFreq = f (covFreq c) }

addPath :: Path k -> Coverage k -> Coverage k
addPath p = onPaths (Freq.inc p)

bucket :: Bucketing -> Coverage k -> Coverage k
bucket b = onPaths (Freq.map (Bucket.bucket b))

lastLoc :: Coverage k -> Text
lastLoc = covLastLoc

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
          let txtLoc = Text.pack (maybe "<unknown>" show loc)
          let msg = Text.pack (show (C.handleName handle)) <> " " <> txtLoc
          liftIO $ Log.debug msg
          let blkId = BlockId.new (C.handleName handle) blockIdInt
          path <- IORef.readIORef pathRef
          let path' = Path.snoc path blkId
          IORef.writeIORef pathRef path'
          IORef.modifyIORef coverRef (addPath path')
          unless (loc == Just What4.InternalPos) $
            IORef.modifyIORef coverRef (\c -> c { covLastLoc = txtLoc })
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
