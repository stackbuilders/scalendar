module SCalendarTest.Helpers
  ( getUTCdayNum
  , testIdentifiers
  , startDay
  ) where


import Data.Time.Clock (UTCTime (..))
import Data.Time.Calendar (toGregorian)
import Data.Text (Text)
import qualified Data.Text as T (pack)


testIdentifiers :: [Text]
testIdentifiers =  T.pack <$> (show <$> [1.. 100])

getUTCdayNum :: UTCTime -> Int
getUTCdayNum (UTCTime day _) =
  let (_, _, num) = toGregorian day
  in num

startDay :: (Integer, Int, Int)
startDay = (1970, 1, 1)
