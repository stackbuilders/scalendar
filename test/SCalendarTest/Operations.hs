module SCalendarTest.Operations where


import Time.SCalendar.Operations ( reserveManyPeriods
                                 , reservePeriod'
                                 , cancelManyPeriods
                                 , isQuantityAvailable,
                                   isReservAvailable  )
import SCalendarTest.Helpers (getUTCdayNum, testIdentifiers)
import SCalendarTest.Arbitrary (CalendarReservations(..))
import Data.Maybe (isJust, fromMaybe)
import Time.SCalendar.Internal ( goToNode
                               , getQMax
                               , getZipInterval )
import Time.SCalendar.Zippers (goLeft, goRight, goUp)
import Time.SCalendar.Types ( Calendar(..)
                            , SCalendar(..)
                            , Reservation(..)
                            , getFrom
                            , getTo
                            , makeCancellation )
import qualified Data.Set as S (isSubsetOf, fromList, size)


symmetricalIntervalLength :: Calendar -> Bool
symmetricalIntervalLength calendar =
  fromMaybe False (checkSimmetry calZipper)
  where
    calZipper = (calendar, [])
    -- ^ --
    checkSimmetry (Unit{}, _) = Just True
    checkSimmetry zipper = do
      leftChild <- goLeft zipper
      rightChild <- goRight zipper
      let i1 = getZipInterval leftChild
          i2 = getZipInterval rightChild
          intervalSymmetry = getUTCdayNum (getTo i1) - getUTCdayNum (getFrom i1)
                             == getUTCdayNum (getTo i2) - getUTCdayNum (getFrom i2)
      return $ intervalSymmetry &&
               fromMaybe False (checkSimmetry leftChild) &&
               fromMaybe False (checkSimmetry rightChild)

qMaxOfParentIncludedInChildren :: CalendarReservations -> Bool
qMaxOfParentIncludedInChildren (CalReservs calendar reservs) = fromMaybe False $ do
  (SCalendar _ calendar') <- reserveManyPeriods reservs (SCalendar (S.fromList testIdentifiers) calendar)
  checks <- sequence $ filter isJust (checkQmax calendar' . reservPeriod <$> reservs)
  return $ and checks
  where
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
quantityNotAvailableAfterReservation (CalReservs _ []) = False
quantityNotAvailableAfterReservation (CalReservs calendar (reserv:_)) = fromMaybe False $ do
  calendar' <- reservePeriod' reserv calendar
  return $ not $
    isQuantityAvailable (totalUnits - S.size (reservUnits reserv) + 1)
                        (reservPeriod reserv)
                        (SCalendar (S.fromList testIdentifiers) calendar')
  where
    totalUnits = length testIdentifiers

periodNotAvailableAfterReservation :: CalendarReservations -> Bool
periodNotAvailableAfterReservation (CalReservs _ []) = False
periodNotAvailableAfterReservation (CalReservs calendar (reserv:_)) = fromMaybe False $ do
  calendar' <- reservePeriod' reserv calendar
  return $ not $
    isReservAvailable reserv (SCalendar (S.fromList testIdentifiers) calendar') &&
    S.size (reservUnits reserv) > 0

reservAvailableAfterCancellation :: CalendarReservations -> Bool
reservAvailableAfterCancellation (CalReservs _ []) = False
reservAvailableAfterCancellation (CalReservs calendar reservs) = fromMaybe False $ do
  (SCalendar _ calendar') <- reserveManyPeriods reservs
                                                (SCalendar (S.fromList testIdentifiers) calendar)
  cancellations <- mapM reservToCanc reservs
  calendar'' <- cancelManyPeriods cancellations calendar'
  return $ and $
    flip isReservAvailable (SCalendar (S.fromList testIdentifiers) calendar'') <$> reservs
  where
    reservToCanc reserv  = makeCancellation (reservPeriod reserv) (reservUnits reserv)
