module SCalendarTest.Internal where


import Time.SCalendar.Internal ( powerOfTwo
                               , isIncluded
                               , calendarSize )


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
