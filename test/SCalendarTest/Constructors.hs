module SCalendarTest.Constructors where


import Time.SCalendar.Operations (augmentCalendar)
import SCalendarTest.Helpers (startDay)
import Data.Maybe (fromMaybe)
import Time.SCalendar.Internal ( daysBetween
                               , getInterval )
import Time.SCalendar.Types ( SCalendar(..)
                            , powerOfTwo
                            , createCalendar
                            , getFrom
                            , getTo       )
import qualified Data.Set as S (empty)


calendarSizePowerOfTwo :: Int -> Bool
calendarSizePowerOfTwo n = fromMaybe False $ do
  let (year, month, day) = startDay
      size = if n > 1 then n else abs n + 2
  calendar <- createCalendar year month day size
  let i = getInterval calendar
  return $ daysBetween (getFrom i) (getTo i) == 2 ^ powerOfTwo size

augmentedCalendarPowerOfKPlusN :: Int -> Int -> Bool
augmentedCalendarPowerOfKPlusN n k = fromMaybe False $ do
  let (year, month, day) = startDay
      n' = powerOfTwo n
      k' = powerOfTwo k
  calendar <- createCalendar year month day n'
  (SCalendar _ calendar') <- augmentCalendar (SCalendar S.empty calendar) k'
  let i = getInterval calendar
      j = getInterval calendar'
  return $ daysBetween (getFrom j) (getTo j) == daysBetween (getFrom i) (getTo i) * (2 ^ k')
