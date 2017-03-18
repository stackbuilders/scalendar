module SCalendarTest.Internal where


import Data.List (elem)
import Data.Maybe (isJust)
import SCalendarTest.Helpers (getUTCdayNum)
import Time.SCalendar.DataTypes (Calendar(..))
import Time.SCalendar.Zippers (goUp)
import SCalendarTest.Arbitrary ( Interval(..)
                               , RandomZippers(..)
                               , CalendarInterval(..) )
import Time.SCalendar.Internal ( powerOfTwo
                               , isIncluded
                               , goToNode
                               , leftMostTopNode
                               , rightMostTopNode
                               , topMostNodes
                               , getZipInterval
                               , getInterval
                               , commonParent   )


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
    checkTarget (TimeUnit unit q qn) = (unit, unit) == interval
    checkTarget (Empty interval') = interval' == interval
    checkTarget (Node interval' q qn c1 c2) = interval' == interval
    -- << >> --
    ifNothing (Node interval' q qn c1 c2) =
      not $ isIncluded interval interval' && (getUTCdayNum from) `mod` 2 == 0
    ifNothing _ = False

isLeftMostTopNode :: Interval -> Calendar -> Bool
isLeftMostTopNode (Interval from to) calendar = maybe True id $ do
  (from', to') <- getZipInterval <$> leftMostTopNode (from, to) calendar
  return $ from' == from &&
           if to' == to
           then (from', to') == (from, to)
           else to' < to

isRightMostTopNode :: Interval -> Calendar -> Bool
isRightMostTopNode (Interval from to) calendar = maybe True id $ do
  (from', to') <- getZipInterval <$> rightMostTopNode (from, to) calendar
  return $ to' == to &&
           if from' == from
           then (from', to') == (from, to)
           else from' > from

returnsCommonParent :: RandomZippers -> Bool
returnsCommonParent (RandomZippers zip1 zip2) = maybe False id $ do
  parent <- commonParent zip1 zip2
  let (from1, to1) = getZipInterval zip1
      (from2, to2) = getZipInterval zip2
      (fromP, toP) = getZipInterval parent
  return $ fromP <= from1 &&
           fromP <= from2 &&
           toP >= to1 &&
           toP >= to2

leftMostAndRightMostInTopMost :: CalendarInterval -> Bool
leftMostAndRightMostInTopMost (CalInterval calendar (Interval from to)) =
  maybe False id $ do
    ltmInterval <- getZipInterval <$> leftMostTopNode (from, to) calendar
    rtmInterval <- getZipInterval <$> rightMostTopNode (from, to) calendar
    topMostIntervals <- (fmap . fmap) getZipInterval (topMostNodes (from, to) calendar)
    return $ (ltmInterval `elem` topMostIntervals) && (rtmInterval `elem` topMostIntervals)

outerMostNodesIncludeIntermediate :: CalendarInterval -> Bool
outerMostNodesIncludeIntermediate (CalInterval calendar (Interval from to)) =
  maybe False id $ do
    (from', _) <- getZipInterval <$> leftMostTopNode (from, to) calendar
    (_, to') <- getZipInterval <$> rightMostTopNode (from, to) calendar
    topMostIntervals <- (fmap . fmap) getZipInterval (topMostNodes (from, to) calendar)
    -- Each intermediate interval must be included in the leftmost and rightmost ones
    return $ all (`isIncluded` (from', to'))  topMostIntervals

ifOnlyOneTopNodeItEqualsInterval :: CalendarInterval -> Bool
ifOnlyOneTopNodeItEqualsInterval (CalInterval calendar (Interval from to)) =
  maybe False id $ do
    topMostIntervals <- (fmap . fmap) getZipInterval (topMostNodes (from, to) calendar)
    if length topMostIntervals == 1
    then return $ (topMostIntervals !! 0) == (from, to)
    else return True

parentOfTopNodesNotIncluded :: CalendarInterval -> Bool
parentOfTopNodesNotIncluded (CalInterval calendar (Interval from to)) =
  maybe False id $ do
    (from', _) <- getZipInterval <$> leftMostTopNode (from, to) calendar
    (_, to') <- getZipInterval <$> rightMostTopNode (from, to) calendar
    tmNodes <- topMostNodes (from, to) calendar
    parentIntervals <- (fmap . fmap) getZipInterval
                                    (sequence $ filter isJust (goUp <$> tmNodes))
    return $ all (`notIncluded` (from', to')) parentIntervals
  where
    notIncluded i1 i2 = not $ isIncluded i1 i2
