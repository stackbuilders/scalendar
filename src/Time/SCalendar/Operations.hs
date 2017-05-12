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


-- | Given a calendar of size 2^n, this function augments that calendar k times,
--   that is, 2^(n+k). The new calendar is  properly updated.
augmentCalendar :: SCalendar -> Int -> Maybe SCalendar
augmentCalendar _ k
  | k <= 0 = Nothing
augmentCalendar scal k = do
  let interval = getInterval $ calendar scal
      (from, to) = (getFrom interval, getTo interval)
      (UTCTime gregDay _) = from
      (year, month, day) = toGregorian gregDay
      newSize = daysBetween from to * (2^k)
  -- ^ create a bigger calendar with a space for our smaller calendar
  largerCal <- createCalendar year month day newSize
  (_, bs) <- goToNode interval largerCal
  -- ^ put the smaller calendar in the slot and update the larger calendar
  updatedCal <- updateQ (calendar scal, bs)
  (root, _) <- upToRoot updatedCal
  return $ SCalendar (calUnits scal) root

-- | Given an interval, an amount of units to be reserved, the number of
--   available units and a calendar this function determines if that period of time
--   and quantity are available in that calendar.
isQuantityAvailable :: Int -> TimePeriod -> SCalendar -> Bool
isQuantityAvailable quant interval scal
  | S.null (calUnits scal) = False
  | quant <= 0 = False
  | quant > S.size (calUnits scal) = False
  | not $ intervalFitsCalendar interval (calendar scal) = False
  | otherwise = checkQuantAvailability (toTimeUnit interval) quant (calUnits scal) (calendar scal, [])

-- | Given a Reservation, and a SCalendar this function determines if that reservation is
--   available in that calendar.
isReservAvailable :: Reservation -> SCalendar -> Bool
isReservAvailable reservation scal
  | S.null (calUnits scal) = False
  | not $ S.isSubsetOf (reservUnits reservation) (calUnits scal) = False
  | not $ intervalFitsCalendar (reservPeriod reservation) (calendar scal) = False
  | otherwise = checkReservAvailability reservation (calUnits scal) (calendar scal, [])

-- | This function inserts reservations into a calendar without any constraint. This function
--   is useful if you want to insert reservations which are not included in the current
--   TotalUnits of an SCalendar.
reservePeriod' :: Reservation -> Calendar -> Maybe Calendar
reservePeriod' reservation calendar = do
  let interval = (toTimeUnit . reservPeriod) reservation
  tmNodes <- topMostNodes interval calendar
  let tmIntervals = fmap getZipInterval tmNodes
  updateCalendar tmIntervals (reservUnits reservation) calendar (\x y -> Just $ S.union x y)

-- | This is like reservePeriod_ but reserves many periods at once.
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

-- | Given a period of time, a set of units to be reserved, and a SCalendar
--   this function returns a new Calendar with a a reservation over that period of
--   time if it is available. The SCalendar returned by this function is a root Node.
reservePeriod :: Reservation -> SCalendar -> Maybe SCalendar
reservePeriod reservation scalendar
  | not $ isReservAvailable reservation scalendar = Nothing
reservePeriod reservation scal = do
  updatedCalendar <- reservePeriod' reservation (calendar scal)
  return $ SCalendar (calUnits scal) updatedCalendar

-- | This function is like reservePeriod, but instead of making one reservation at a time,
--   it takes a list of reservations. This function will return a calendar only with the ones
--   that pass the isReservAvailable test. Take into account that reservations will be inserted
--   in the tree in the order they are in the input list. So, if a reservation conflicts with the
--   ones that have been alredy inserted, it will not be included in the tree.
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

-- | This operation takes a Cancellation and returns a new calendar with that Cancellation
--   subtracted from the top-nodes of that Cancellation (Q is therefore updated all over the tree).
--   Be careful with this operation: Two reservations might have the same top nodes, so you
--   must have a way to keep track which elements belong to one reservation and to the other one.
--   deletion in your data base.
--   Note that deleting units from a tree does not prevent you from deleting from a reservation
--   that has never been made. For example, if you have previously reserved n units for (2,7), that
--   reservation will be affected if you delete from a period of time like (2,5). That's why whenever you
--   subtract units from a tree, you must be certain that the period of time has been previously reserved.
--   Also, note that you cannot delete more units than QN, that is, if
--   (size unitsToDelete) > (size QN(node)), a Nothing will be propagated.
cancelPeriod :: Cancellation -> Calendar -> Maybe Calendar
cancelPeriod cancellation calendar = do
  -- ^ To delete from  a previous reservation, we must know its top-nodes.
  tmNodes <- topMostNodes (cancPeriod cancellation) calendar
  let tmIntervals = fmap getZipInterval tmNodes
  updateCalendar tmIntervals (cancUnits cancellation) calendar diff
  where
    diff x y
      | not $ S.isSubsetOf y x = Nothing
      | otherwise = Just (S.difference x y)

-- | This is like cancelPeriod but cancels many periods at once.
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

-- | Given a period of time and a Calendar, this function returns a Report which
--   summarizes important data about that period of time.
periodReport :: TimePeriod -> SCalendar -> Maybe Report
periodReport interval scal = do
  guard $ intervalFitsCalendar interval (calendar scal)
  tmNodes <- topMostNodes (toTimeUnit interval) (calendar scal)
  qMaxs <- mapM getQMax tmNodes
  let sQMax =  S.unions qMaxs
  return $ Report interval (calUnits scal) sQMax (S.difference (calUnits scal) sQMax)
