module SCalendarTest.Internal where


import SCalendarTest.Arbitrary (Interval(..))
import Data.Time.Clock (UTCTime (..))
import Time.SCalendar.DataTypes (Calendar(..))
import Data.Time.Calendar (toGregorian)
import Time.SCalendar.Internal ( powerOfTwo
                               , isIncluded
                               , goToNode   )


alwaysGreateOrEqualThanN :: Int -> Bool
alwaysGreateOrEqualThanN n = 2^(powerOfTwo n) >= n

eqIntervalsIfIncludeEachOther :: (Int, Int) -> (Int, Int) -> Bool
eqIntervalsIfIncludeEachOther r@(r1, r2) t@(t1, t2)
  | isIncluded r t && isIncluded t r = r1 == t1 && r2 == t2
  | isIncluded r t = not $ isIncluded t r
  | isIncluded t r = not $ isIncluded r t
  | not (isIncluded r t) && not (isIncluded t r) && wellFormed = r1 < t2 || t1 < r2
  | otherwise = not wellFormed
  where
    wellFormed = r1 <= r2 && t1 <= t2

returnsTargetZipper :: Calendar -> Interval -> Bool
returnsTargetZipper calendar (Interval from to) =
  let maybeCalendar = fst <$> goToNode interval calendar
  in maybe (ifNothing calendar) checkTarget maybeCalendar
  where
    interval = (from, to)
    -- << >> --
    getUTCdayNum (UTCTime day _) =
      let (_, num, _) = toGregorian day
      in num
    -- << >> --
    checkTarget (TimeUnit unit q qn) = (unit, unit) == interval
    checkTarget (Empty interval') = interval' == interval
    checkTarget (Node interval' q qn c1 c2) = interval' == interval
    -- << >> --
    ifNothing (Node interval' q qn c1 c2) =
      not $ isIncluded interval interval' && (getUTCdayNum from) `mod` 2 == 0
    ifNothing _ = False
