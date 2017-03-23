module SCalendarTest.Operations where


import Time.SCalendar.Operations ( createCalendar
                                 , reserveManyPeriods
                                 , reservePeriod_
                                 , cancelManyPeriods
                                 , isQuantityAvailable
                                 , isReservAvailable )
import SCalendarTest.Helpers ( getUTCdayNum
                             , testIdentifiers )
import Data.Maybe (isJust)
import Time.SCalendar.Internal ( calendarSize
                               , goToNode
                               , powerOfTwo
                               , getQMax
                               , getZipInterval )
import SCalendarTest.Arbitrary (CalendarReservations(..))
import Time.SCalendar.Zippers (goLeft, goRight, goUp)
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Clock (UTCTime(..))
import Time.SCalendar.DataTypes ( Calendar(..)
                                , SCalendar(..)
                                , Reservation(..)
                                , Cancellation(..) )
import qualified Data.Set as S (empty, isSubsetOf, fromList, size)


calendarSizePowerOfTwo :: Int -> Bool
calendarSizePowerOfTwo n =
  let size = if n > 1 then n else (abs n) + 2
      startDay = UTCTime (fromGregorian 1970 1 1) 0
      calendar = maybe (TimeUnit startDay S.empty S.empty)
                       id
                       (createCalendar startDay size)
  in calendarSize calendar == 2 ^ (powerOfTwo size)

symmetricalIntervalLength :: Calendar -> Bool
symmetricalIntervalLength calendar =
  maybe False id (checkSimmetry calZipper)
  where
    calZipper = (calendar, [])
    -- ^ --
    checkSimmetry ((TimeUnit _ _ _), _) = Just True
    checkSimmetry (Empty _, _) = Just True
    checkSimmetry zipper = do
      leftChild <- goLeft zipper
      rightChild <- goRight zipper
      let (from1, to1) = getZipInterval leftChild
          (from2, to2) = getZipInterval rightChild
          intervalSymmetry = (getUTCdayNum to1) - (getUTCdayNum from1)
                             == (getUTCdayNum to2) - (getUTCdayNum from2)
      return $ intervalSymmetry &&
               maybe False id (checkSimmetry leftChild) &&
               maybe False id (checkSimmetry rightChild)

qMaxOfParentIncludedInChildren :: CalendarReservations -> Bool
qMaxOfParentIncludedInChildren (CalReservs calendar reservs) = maybe False id $ do
  (SCalendar _ calendar') <- reserveManyPeriods reservs (SCalendar (S.fromList testIdentifiers) calendar)
  checks <- sequence $ filter isJust ((checkQmax calendar') . reservToInterval <$> reservs)
  return $ and checks
  where
    reservToInterval (Reservation _ interval) = interval
    -- ^ --
    checkQmax cal interval = do
      zipper <- goToNode interval cal
      zipParent <- goUp zipper
      zipLChild <- goLeft zipper
      qMax <- getQMax zipper
      qMaxParent <- getQMax zipParent
      qMaxLChild <- getQMax zipLChild
      return $ qMaxParent `S.isSubsetOf` qMax &&
               qMax `S.isSubsetOf` qMaxLChild

quantityNotAvailableAfterReservation :: CalendarReservations -> Bool
quantityNotAvailableAfterReservation (CalReservs calendar []) = False
quantityNotAvailableAfterReservation (CalReservs calendar (reserv:_)) = maybe False id $ do
  calendar' <- reservePeriod_ reserv calendar
  return $ not $
    isQuantityAvailable (totalUnits - (S.size units) + 1)
                        interval
                        (SCalendar (S.fromList testIdentifiers) calendar')
  where
    (Reservation units interval) = reserv
    -- ^ --
    totalUnits = length testIdentifiers

periodNotAvailableAfterReservation :: CalendarReservations -> Bool
periodNotAvailableAfterReservation (CalReservs calendar []) = False
periodNotAvailableAfterReservation (CalReservs calendar (reserv:_)) = maybe False id $ do
  calendar' <- reservePeriod_ reserv calendar
  return $ not $
    isReservAvailable reserv (SCalendar (S.fromList testIdentifiers) calendar') &&
    (S.size units) > 0
  where
    (Reservation units _) = reserv
    -- ^ --
    totalUnits = length testIdentifiers

reservAvailableAfterCancellation :: CalendarReservations -> Bool
reservAvailableAfterCancellation (CalReservs calendar []) = False
reservAvailableAfterCancellation (CalReservs calendar reservs) = maybe False id $ do
  (SCalendar _ calendar') <- reserveManyPeriods reservs
                                                (SCalendar (S.fromList testIdentifiers) calendar)
  calendar'' <- cancelManyPeriods (reservToCanc <$> reservs) calendar'
  return $ and $
    (flip isReservAvailable) (SCalendar (S.fromList testIdentifiers) calendar'') <$> reservs
  where
    reservToCanc (Reservation units interval) = Cancellation units interval
