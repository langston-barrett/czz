{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Czz.State
  ( State
  , new
  , newIO
  , record
  , stats
  , tries
  , pool
  , hasBug
  , summarize
  )
where

import qualified Data.Maybe as Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Numeric.Natural (Natural)

import qualified What4.ProgramLoc as What4

import           Czz.Coverage.Bucket.Bucketed (BucketedCoverage)
import qualified Czz.Coverage.Bucket.Bucketed as Bucketed
import           Czz.Coverage.Bucket.Bucketing (Bucketing)
import           Czz.KLimited (IsKLimited)
import           Czz.Now (Now)
import qualified Czz.Now as Now
import           Czz.Record (Record)
import qualified Czz.Record as Rec
import qualified Czz.Result as Res
import           Czz.State.Stats (Stats)
import qualified Czz.State.Stats as Stats

--------------------------------------------------------------------------------
-- State

-- | Invariant: sCover is the coverage of all sPool
data State env eff k fb
  = State
    { sCoverage :: BucketedCoverage k
    , sStats :: !Stats
    , sPool :: Seq (Record env eff k fb)
    , sTries :: !Natural
    }
  deriving (Eq, Functor, Ord)

new :: IsKLimited k => Now -> State env eff k fb
new now =
  State
  { sCoverage = Bucketed.empty
  , sPool = Seq.empty
  , sTries = 0
  , sStats = Stats.new now
  }

newIO :: IsKLimited k => IO (State env eff k fb)
newIO = new <$> Now.now

pool :: State env eff k fb -> Seq (Record env eff k fb)
pool = sPool

hasBug :: State env eff k fb -> Bool
hasBug = any Rec.hasBug . sPool

stats :: State env eff k fb -> Stats
stats = sStats

tries :: State env eff k fb -> Natural
tries = Stats.tries . stats

-- | Read docs on coverage tracking for a high-level description.
record ::
  Bucketing ->
  Record env eff k fb ->
  State env eff k fb ->
  IO (Bool, State env eff k fb)
record bk r state = do
  now <- Now.now
  let bug = Rec.hasBug r
      bucketed = Bucketed.bucket bk (Rec.coverage r)
      (totalCover, isNew) = Bucketed.merge bucketed (sCoverage state)
      stats' = Stats.record now isNew bug (Rec.missing r) (sStats state)
  return $
    if not isNew
    then
      ( False
      , state { sStats = stats' }
      )
    else
      ( True
      , state
        { sCoverage = totalCover
        , sStats = stats'
        , sPool = sPool state Seq.|> r
        }
      )

summarize ::
  State env eff k fb ->
  Text
summarize st =
  let
      getBugLoc =
        \case
          Res.Bug loc _ -> Just (What4.plSourceLoc loc)
          _ -> Nothing
      results = Set.unions (fmap Rec.result (pool st))
      bugs = Maybe.mapMaybe getBugLoc (Set.toList results)
  in Text.unlines ("Found these bugs:":map (("- " <>) . Text.pack . show) bugs)
