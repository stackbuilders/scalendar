module SCalendarTest.Operations where


import Time.SCalendar.Operations (createCalendar)
import Time.SCalendar.Internal (calendarSize, powerOfTwo)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Time.SCalendar.DataTypes (Calendar(..))
import qualified Data.Set as S (empty)


calendarSizePowerOfTwo :: Int -> Bool
calendarSizePowerOfTwo n =
  let size = if n > 1 then n else (abs n) + 2
      startDay = UTCTime (fromGregorian 1970 1 1) 0
      calendar = maybe (TimeUnit startDay S.empty S.empty)
                       id
                       (createCalendar startDay size)
  in calendarSize calendar == 2 ^ (powerOfTwo size)
