{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Czz.State
  ( State
  , new
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

import qualified What4.ProgramLoc as What4

import           Czz.Record (Record)
import qualified Czz.Record as Rec
import qualified Czz.Result as Res

-- | Invariant: sCover is the coverage of all sPool
data State env eff fb
  = State
    { -- | Hashes of all the feedback attached to records
      sFeedback :: IntSet
    , sPool :: Seq (Record env eff fb)
    , sTries :: !Int
    }
  deriving (Eq, Functor, Ord)

new :: State env eff fb
new =
  State
    { sFeedback = IntSet.empty
    , sPool = Seq.empty
    , sTries = 0
    }

record ::
  Record env eff fb ->
  State env eff fb ->
  (Bool, State env eff fb)
record r state =
  let fId = Hash.hash (Rec.feedbackId r, Rec.hasBug r)
  in if fId `IntSet.member` sFeedback state
     then ( False
          , state { sTries = 1 + sTries state }
          )
     else ( True
          , state
            { sFeedback = IntSet.insert fId (sFeedback state)
            , sPool = sPool state Seq.|> r
            , sTries = 0
            }
          )

tries :: State env eff fb -> Int
tries = sTries

pool :: State env eff fb -> Seq (Record env eff fb)
pool = sPool

hasBug :: State env eff fb -> Bool
hasBug = any Rec.hasBug . sPool

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
