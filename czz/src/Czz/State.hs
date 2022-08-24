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

import qualified Data.Hashable as Hash
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Maybe as Maybe
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (UTCTime)
import qualified Data.Time as Time
import           Numeric.Natural (Natural)

import qualified What4.ProgramLoc as What4

import           Czz.Record (Record)
import qualified Czz.Record as Rec
import qualified Czz.Result as Res
import           Czz.State.Stats (Stats)
import qualified Czz.State.Stats as Stats

--------------------------------------------------------------------------------
-- State

-- | Invariant: sCover is the coverage of all sPool
data State env eff fb
  = State
    { sStats :: !Stats
      -- | Hashes of all the feedback attached to records
    , sFeedback :: IntSet
    , sPool :: Seq (Record env eff fb)
    , sTries :: !Natural
    }
  deriving (Eq, Functor, Ord)

new :: UTCTime -> State env eff fb
new now =
  State
  { sFeedback = IntSet.empty
  , sPool = Seq.empty
  , sTries = 0
  , sStats = Stats.new now
  }

newIO :: IO (State env eff fb)
newIO = new <$> Time.getCurrentTime

pool :: State env eff fb -> Seq (Record env eff fb)
pool = sPool

hasBug :: State env eff fb -> Bool
hasBug = any Rec.hasBug . sPool

stats :: State env eff fb -> Stats
stats = sStats

tries :: State env eff fb -> Natural
tries = Stats.tries . stats

record ::
  Record env eff fb ->
  State env eff fb ->
  IO (Bool, State env eff fb)
record r state =
  let bug = Rec.hasBug r
      fId = Hash.hash (Rec.feedbackId r, bug)
  in if fId `IntSet.member` sFeedback state
     then return ( False
                 , state { sStats = Stats.notNew (sStats state) }
                 )
     else do
       now <- Time.getCurrentTime
       return
         ( True
         , state
           { sStats = Stats.newRec now bug (sStats state)
           , sFeedback = IntSet.insert fId (sFeedback state)
           , sPool = sPool state Seq.|> r
           }
         )

summarize ::
  State env eff fb ->
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
