module Czz.Now
  ( Now
  , now
  , fakeNow
  , getNow
  )
where

import           Data.Time (UTCTime)
import qualified Data.Time as Time

newtype Now
  = Now { _getNow :: UTCTime }
  deriving (Eq, Ord)

now :: IO Now
now = Now <$> Time.getCurrentTime

fakeNow :: UTCTime -> Now
fakeNow = Now

getNow :: Now -> UTCTime
getNow = _getNow
