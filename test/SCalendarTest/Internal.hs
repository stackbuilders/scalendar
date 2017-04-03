module SCalendarTest.Internal where


import Data.List (elem)
import Data.Maybe (isJust, fromMaybe)
import Data.Time (UTCTime(..), toGregorian, diffUTCTime )
import SCalendarTest.Helpers (getUTCdayNum)
import Time.SCalendar.Types ( Calendar(..)
                            , Reservation(..)
                            , TimePeriod
                            , getFrom
                            , getTo
                            , isIncluded
                            , powerOfTwo
                            , oneDay
                            , makeTimePeriod )
import Time.SCalendar.Zippers (goUp)
import SCalendarTest.Arbitrary (RandomZippers(..), CalendarReservations(..))
import Time.SCalendar.Internal ( goToNode
                               , leftMostTopNode
                               , rightMostTopNode
                               , topMostNodes
                               , getZipInterval
                               , commonParent   )


alwaysGreateOrEqualThanN :: Int -> Bool
alwaysGreateOrEqualThanN n = 2^ powerOfTwo n >= n

eqIntervalsIfIncludeEachOther :: TimePeriod -> TimePeriod -> Bool
eqIntervalsIfIncludeEachOther i j
  | isIncluded i j && isIncluded j i = i1 == j1 && i2 == j2
  | isIncluded i j = not $ isIncluded j i
  | isIncluded j i = not $ isIncluded i j
  | not (isIncluded i j) && not (isIncluded j i) && wellFormed = i1 < j2 || j1 < i2
  | otherwise = not wellFormed
  where
    (i1, i2) = (getFrom i, getTo i)
    (j1, j2) = (getFrom j, getTo j)
    wellFormed = i1 <= i2 && j1 <= j2

returnsTargetZipper :: Calendar -> TimePeriod -> Bool
returnsTargetZipper calendar interval =
  let maybeCalendar = fst <$> goToNode interval calendar
  in maybe (ifNothing calendar) checkTarget maybeCalendar
  where
    checkTarget (Unit unit _ _) = unit == interval
    checkTarget (Node interval' _ _ _ _) = interval' == interval
    -- ^ --
    ifNothing (Node interval' _ _ _ _) =
      not $ isIncluded interval interval' && ((getUTCdayNum . getFrom) interval) `mod` 2 == 0
    ifNothing _ = False

isLeftMostTopNode :: CalendarReservations -> Bool
isLeftMostTopNode (CalReservs _ []) = False
isLeftMostTopNode (CalReservs calendar (reserv:_)) = fromMaybe False $ do
  i2 <- getZipInterval <$> leftMostTopNode i1 calendar
  return $ getFrom i2 == getFrom i1 &&
           if getTo i2 == getTo i1
           then  (getFrom i1, getTo i1) == (getFrom i2, getTo i2)
           else getTo i2 < getTo i1
  where
    i1 = reservPeriod reserv

isRightMostTopNode :: CalendarReservations -> Bool
isRightMostTopNode (CalReservs _ []) = False
isRightMostTopNode (CalReservs calendar (reserv:_)) = fromMaybe False $ do
  i2 <- getZipInterval <$> rightMostTopNode i1 calendar
  return $ getTo i2 == getTo i1 &&
           if getFrom i2 == getFrom i1
           then (getFrom i1, getTo i1) == (getFrom i2, getTo i2)
           else getFrom i2 > getFrom i1
  where
    i1 = reservPeriod reserv

returnsCommonParent :: RandomZippers -> Bool
returnsCommonParent (RandomZippers zip1 zip2) = fromMaybe False $ do
  parent <- commonParent zip1 zip2
  let c1 = getZipInterval zip1
      c2 = getZipInterval zip2
      p= getZipInterval parent
  return $ getFrom p <= getFrom c1 &&
           getFrom p <= getFrom c2 &&
           getTo p >= getTo c1 &&
           getTo p >= getTo c2

leftMostAndRightMostInTopMost :: CalendarReservations -> Bool
leftMostAndRightMostInTopMost (CalReservs _ []) = False
leftMostAndRightMostInTopMost (CalReservs calendar (reserv:_)) = fromMaybe False $ do
  ltmInterval <- getZipInterval <$> leftMostTopNode interval calendar
  rtmInterval <- getZipInterval <$> rightMostTopNode interval calendar
  topMostIntervals <- (fmap . fmap) getZipInterval (topMostNodes interval calendar)
  return $ (ltmInterval `elem` topMostIntervals) && (rtmInterval `elem` topMostIntervals)
  where
    interval = reservPeriod reserv

outerMostNodesIncludeIntermediate :: CalendarReservations -> Bool
outerMostNodesIncludeIntermediate (CalReservs _ []) = False
outerMostNodesIncludeIntermediate (CalReservs calendar (reserv:_)) = fromMaybe False $ do
  from' <- (getFrom . getZipInterval) <$> leftMostTopNode interval calendar
  to' <- (getTo . getZipInterval) <$> rightMostTopNode interval calendar
  topMostIntervals <- (fmap . fmap) getZipInterval (topMostNodes interval calendar)
  -- ^ Each intermediate interval must be included in the leftmost and rightmost ones
  let numDays = round $ diffUTCTime to' from' / oneDay
      (UTCTime gregDay _) = from'
      (year, month, day) = toGregorian gregDay
  timePeriod <- makeTimePeriod year month day numDays
  return $ all (`isIncluded` timePeriod)  topMostIntervals || timePeriod `elem` topMostIntervals
  where
    interval = reservPeriod reserv

ifOnlyOneTopNodeItEqualsInterval :: CalendarReservations -> Bool
ifOnlyOneTopNodeItEqualsInterval (CalReservs _ []) = False
ifOnlyOneTopNodeItEqualsInterval (CalReservs calendar (reserv:_)) = fromMaybe False $ do
  topMostIntervals <- (fmap . fmap) getZipInterval (topMostNodes interval calendar)
  if length topMostIntervals == 1
  then return $ head topMostIntervals == interval
  else return True
  where
    interval = reservPeriod reserv

parentOfTopNodesNotIncluded :: CalendarReservations -> Bool
parentOfTopNodesNotIncluded (CalReservs _ []) = False
parentOfTopNodesNotIncluded (CalReservs calendar (reserv:_)) = fromMaybe False $ do
  from' <- (getFrom . getZipInterval) <$> leftMostTopNode interval calendar
  to' <- (getTo . getZipInterval) <$> rightMostTopNode interval calendar
  tmNodes <- topMostNodes interval calendar
  parentIntervals <- (fmap . fmap) getZipInterval
                                  (sequence $ filter isJust (goUp <$> tmNodes))
  let numDays = round $ diffUTCTime to' from' / oneDay
      (UTCTime gregDay _) = from'
      (year, month, day) = toGregorian gregDay
  timePeriod <- makeTimePeriod year month day numDays
  return $ all (`notIncluded` timePeriod) parentIntervals
  where
    notIncluded i1 i2 = not $ isIncluded i1 i2
    -- ^ --
    interval = reservPeriod reserv
