module Czz.State.Stats
  ( Stats
  , new
  , execs
  , execsPerSec
  , start
  , lastNew
  , lastBug
  , tries
  , notNew
  , newRec
  )
where

import qualified Data.Fixed as Fixed
import           Data.Time (NominalDiffTime, UTCTime)
import qualified Data.Time as Time
import           Numeric.Natural (Natural)

data Stats
  = Stats
    { sExecs :: !Natural
    , sStart :: !UTCTime
    , sLastNew :: !UTCTime
    , sLastBug :: !(Maybe UTCTime)
    , sPoolSize :: !Natural
    , sTries :: !Natural
    }
  deriving (Eq, Ord)

new :: UTCTime -> Stats
new now =
  Stats
  { sExecs = 0
  , sStart = now
  , sLastNew = now
  , sLastBug = Nothing
  , sPoolSize = 0
  , sTries = 0
  }

execs :: Stats -> Natural
execs = sExecs

execsPerSec :: Stats -> UTCTime -> Natural
execsPerSec s now =
  -- "Conversion functions such as fromInteger [...] will treat it as seconds."
  let diff = Time.nominalDiffTimeToSeconds (start s `Time.diffUTCTime` now)
      Fixed.MkFixed ps = diff
      secs = ps `div` Fixed.resolution diff
  in execs s `div` fromIntegral secs

start :: Stats -> UTCTime
start = sStart

lastNew :: Stats -> NominalDiffTime
lastNew s = sStart s `Time.diffUTCTime` sLastNew s

lastBug :: Stats -> Maybe NominalDiffTime
lastBug s = (sStart s `Time.diffUTCTime`) <$> sLastBug s

tries :: Stats -> Natural
tries = sTries

notNew :: Stats -> Stats
notNew s =
  s
  { sExecs = 1 + sExecs s
  , sTries = 1 + sTries s
  }

newRec :: UTCTime -> Bool -> Stats -> Stats
newRec now isBug s =
  s
  { sExecs = 1 + sExecs s
  , sLastNew = now
  , sLastBug = if isBug then Just now else sLastBug s
  , sTries = 0
  }
