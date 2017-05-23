module Time.SCalendar.Operations
  ( augmentCalendar
  , isQuantityAvailable
  , isReservAvailable
  , reservePeriod'
  , reservPeriod
  , reserveManyPeriods
  , reserveManyPeriods'
  , cancelPeriod
  , cancelManyPeriods
  , periodReport
  ) where


import Data.Maybe (isNothing)
import Time.SCalendar.Zippers
import Time.SCalendar.Types
import Data.Time (UTCTime(..), toGregorian)
import Control.Monad (guard)
import Time.SCalendar.Internal
import qualified Data.Set as S ( null
                               , size
                               , difference
                               , isSubsetOf
                               , union
                               , unions   )

-- | Given an SCalendar of size 2^n, this function increases its size k times, that is,
-- 2^(n+k). The new SCalendar is properly updated up to its root so that it will render
-- the same results as the previous one. For example, given an SCalendar `c` of size 2^5=32,
-- 'augmentCalendar c 3' would produce a new SCalendar of size 2^(5+3)=256.
augmentCalendar :: SCalendar -- ^ SCalendar to be augmented.
                -> Int -- ^ Number of times by which the SCalendar will be augmented.
                -> Maybe SCalendar
augmentCalendar _ k
  | k <= 0 = Nothing
augmentCalendar scal k = do
  let interval = getInterval $ calendar scal
      (from, to) = (getFrom interval, getTo interval)
      (UTCTime gregDay _) = from
      (year, month, day) = toGregorian gregDay
      newSize = daysBetween from to * (2^k)
  largerCal <- createCalendar year month day newSize
  (_, bs) <- goToNode interval largerCal
  updatedCal <- updateQ (calendar scal, bs)
  (root, _) <- upToRoot updatedCal
  return $ SCalendar (calUnits scal) root

-- | Given a quantity, this function determines if it is available in a TimePeriod for a
-- specific SCalendar. Thus, it does not take into account the particular resources whose
-- availability wants to be determined: it is only concerned with the availabilty of a quantity
-- in a specific SCalendar.
isQuantityAvailable :: Int -- ^ Quantity of resources.
                    -> TimePeriod -- ^ TimePeriod over which we want to determine the availability of
                                  -- the quantity.
                    -> SCalendar -- ^ SCalendar over which we want to determine the availability of
                                 -- the quantity in a Given TimePeriod.
                    -> Bool
isQuantityAvailable quant interval scal
  | S.null (calUnits scal) = False
  | quant <= 0 = False
  | quant > S.size (calUnits scal) = False
  | not $ intervalFitsCalendar interval (calendar scal) = False
  | otherwise = checkQuantAvailability (toTimeUnit interval) quant (calUnits scal) (calendar scal, [])

-- | Given a Reservation, this function determines if it is available in a SCalendar. A
-- Reservation is the product of a set of identifiers which point to reservable resources
-- and a TimePeriod over which those resources are to be reserved. Thus, this function
-- checks if that particular set of resources is available for a TimePeriod in the given SCalendar.
isReservAvailable :: Reservation -> SCalendar -> Bool
isReservAvailable reservation scal
  | S.null (calUnits scal) = False
  | not $ S.isSubsetOf (reservUnits reservation) (calUnits scal) = False
  | not $ intervalFitsCalendar (reservPeriod reservation) (calendar scal) = False
  | otherwise = checkReservAvailability reservation (calUnits scal) (calendar scal, [])

reservePeriod' :: Reservation -> Calendar -> Maybe Calendar
reservePeriod' reservation calendar = do
  let interval = (toTimeUnit . reservPeriod) reservation
  tmNodes <- topMostNodes interval calendar
  let tmIntervals = fmap getZipInterval tmNodes
  updateCalendar tmIntervals (reservUnits reservation) calendar (\x y -> Just $ S.union x y)

reserveManyPeriods' :: [Reservation] -> Calendar -> Maybe Calendar
reserveManyPeriods' [] calendar = Just calendar
reserveManyPeriods' (reservation:rs) calendar = do
  updatedCalendar <- addReservation reservation calendar
  reserveManyPeriods' rs updatedCalendar
  where
    addReservation res cal
      | isNothing maybeCalendar = Just cal
      | otherwise = maybeCalendar
      where maybeCalendar = reservePeriod' res cal

reservePeriod :: Reservation -> SCalendar -> Maybe SCalendar
reservePeriod reservation scalendar
  | not $ isReservAvailable reservation scalendar = Nothing
reservePeriod reservation scal = do
  updatedCalendar <- reservePeriod' reservation (calendar scal)
  return $ SCalendar (calUnits scal) updatedCalendar

reserveManyPeriods :: [Reservation] -> SCalendar -> Maybe SCalendar
reserveManyPeriods [] calendar = Just calendar
reserveManyPeriods (reservation:rs) calendar = do
  updatedCalendar <- addReservation reservation calendar
  reserveManyPeriods rs updatedCalendar
  where
    addReservation res uCal
      | isNothing maybeCalendar = Just uCal
      | otherwise = maybeCalendar
      where maybeCalendar = reservePeriod res uCal

cancelPeriod :: Cancellation -> Calendar -> Maybe Calendar
cancelPeriod cancellation calendar = do
  tmNodes <- topMostNodes (cancPeriod cancellation) calendar
  let tmIntervals = fmap getZipInterval tmNodes
  updateCalendar tmIntervals (cancUnits cancellation) calendar diff
  where
    diff x y
      | not $ S.isSubsetOf y x = Nothing
      | otherwise = Just (S.difference x y)

cancelManyPeriods :: [Cancellation] -> Calendar -> Maybe Calendar
cancelManyPeriods [] calendar = Just calendar
cancelManyPeriods (cancellation:cs) calendar = do
  updatedCalendar <- addCancellation cancellation calendar
  cancelManyPeriods cs updatedCalendar
  where
    addCancellation canc cal
      | isNothing maybeCalendar = Just cal
      | otherwise = maybeCalendar
      where maybeCalendar = cancelPeriod canc cal

periodReport :: TimePeriod -> SCalendar -> Maybe Report
periodReport interval scal = do
  guard $ intervalFitsCalendar interval (calendar scal)
  tmNodes <- topMostNodes (toTimeUnit interval) (calendar scal)
  qMaxs <- mapM getQMax tmNodes
  let sQMax =  S.unions qMaxs
  return $ Report interval (calUnits scal) sQMax (S.difference (calUnits scal) sQMax)
