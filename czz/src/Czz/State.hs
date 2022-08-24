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
  , execs
  , start
  , lastNew
  , lastBug
  , record
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
import           Data.Time (NominalDiffTime, UTCTime)
import qualified Data.Time as Time
import           Numeric.Natural (Natural)

import qualified What4.ProgramLoc as What4

import           Czz.Record (Record)
import qualified Czz.Record as Rec
import qualified Czz.Result as Res

-- | Invariant: sCover is the coverage of all sPool
data State env eff fb
  = State
    { sExecs :: !Natural
    , sStart :: UTCTime
    , sLastNew :: UTCTime
    , sLastBug :: Maybe UTCTime
      -- | Hashes of all the feedback attached to records
    , sFeedback :: IntSet
    , sPool :: Seq (Record env eff fb)
    , sTries :: !Natural
    }
  deriving (Eq, Functor, Ord)

new :: UTCTime -> State env eff fb
new t =
  State
  { sExecs = 0
  , sStart = t
  , sLastNew = t
  , sLastBug = Nothing
  , sFeedback = IntSet.empty
  , sPool = Seq.empty
  , sTries = 0
  }

newIO :: IO (State env eff fb)
newIO = new <$> Time.getCurrentTime

execs :: State env eff fb -> Natural
execs = sExecs

start :: State env eff fb -> UTCTime
start = sStart

lastNew :: State env eff fb -> NominalDiffTime
lastNew s = sStart s `Time.diffUTCTime` sLastNew s

lastBug :: State env eff fb -> Maybe NominalDiffTime
lastBug s = (sStart s `Time.diffUTCTime`) <$> sLastBug s

tries :: State env eff fb -> Natural
tries = sTries

pool :: State env eff fb -> Seq (Record env eff fb)
pool = sPool

hasBug :: State env eff fb -> Bool
hasBug = any Rec.hasBug . sPool

record ::
  Record env eff fb ->
  State env eff fb ->
  IO (Bool, State env eff fb)
record r state =
  let bug = Rec.hasBug r
      fId = Hash.hash (Rec.feedbackId r, bug)
  in if fId `IntSet.member` sFeedback state
     then return ( False
                 , state
                   { sExecs = 1 + sExecs state
                   , sTries = 1 + sTries state
                   }
                 )
     else do
       now <- Time.getCurrentTime
       return
         ( True
         , state
           { sExecs = 1 + sExecs state
           , sLastNew = now
           , sLastBug = if bug then Just now else sLastBug state
           , sFeedback = IntSet.insert fId (sFeedback state)
           , sPool = sPool state Seq.|> r
           , sTries = 0
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
