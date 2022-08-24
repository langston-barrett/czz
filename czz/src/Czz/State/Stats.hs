module Czz.State.Stats
  ( Stats
  , new
  , execs
  , execsPerSec
  , start
  , sinceStart
  , sinceLastNew
  , sinceLastBug
  , poolSize
  , tries
  , missing
  , record
  )
where

import qualified Data.Fixed as Fixed
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Time (NominalDiffTime, UTCTime)
import qualified Data.Time as Time
import           Numeric.Natural (Natural)

import           Czz.Now (Now)
import qualified Czz.Now as Now

data Stats
  = Stats
    { sExecs :: !Natural
    , sStart :: !UTCTime
    , sLastNew :: !UTCTime
    , sLastBug :: !(Maybe UTCTime)
    , sPoolSize :: !Natural
    , sTries :: !Natural
      -- TODO(lb): This should get disabled, it's non-constant time
    , sMissing :: Set Text
    }
  deriving (Eq, Ord)

new :: Now -> Stats
new now =
  Stats
  { sExecs = 0
  , sStart = Now.getNow now
  , sLastNew = Now.getNow now
  , sLastBug = Nothing
  , sPoolSize = 0
  , sTries = 0
  , sMissing = Set.empty
  }

execs :: Stats -> Natural
execs = sExecs

execsPerSec :: Stats -> Now -> Natural
execsPerSec s now =
  let diff =
        Time.nominalDiffTimeToSeconds (Now.getNow now `Time.diffUTCTime` start s)
      Fixed.MkFixed ps = diff
      secs = ps `div` Fixed.resolution diff
  in execs s `div` max 1 (fromIntegral secs)

start :: Stats -> UTCTime
start = sStart

sinceStart :: Stats -> Now -> NominalDiffTime
sinceStart s = (`Time.diffUTCTime` start s) . Now.getNow

sinceLastNew :: Stats -> Now -> NominalDiffTime
sinceLastNew s = (`Time.diffUTCTime` sLastNew s) . Now.getNow

sinceLastBug :: Stats -> Now -> Maybe NominalDiffTime
sinceLastBug s =
  case sLastBug s of
    Nothing -> const Nothing
    Just t -> Just . (`Time.diffUTCTime` t) . Now.getNow

poolSize :: Stats -> Natural
poolSize = sPoolSize

tries :: Stats -> Natural
tries = sTries

missing :: Stats -> Set Text
missing = sMissing

record :: Now -> Bool -> Bool -> Set Text -> Stats -> Stats
record now isNew isBug miss s =
  if not isNew
  then s
       { sExecs = 1 + sExecs s
       , sTries = 1 + sTries s
       , sMissing = Set.union (sMissing s) miss
       }
  else s
       { sExecs = 1 + sExecs s
       , sLastNew = Now.getNow now
       , sLastBug = if isBug then Just (Now.getNow now) else sLastBug s
       , sPoolSize = 1 + sPoolSize s
       , sTries = 0
       , sMissing = Set.union (sMissing s) miss
       }
