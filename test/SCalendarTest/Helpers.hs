module SCalendarTest.Helpers where


import Data.Time.Clock (UTCTime (..))
import Data.Time.Calendar (toGregorian)


getUTCdayNum (UTCTime day _) =
  let (_, _, num) = toGregorian day
  in num
