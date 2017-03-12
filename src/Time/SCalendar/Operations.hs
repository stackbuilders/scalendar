module Time.SCalendar.Operations where


import Data.Set (Set)
import Data.Time (NominalDiffTime, addUTCTime)
import Data.Text (Text)
import Time.SCalendar.Zippers
import Time.SCalendar.DataTypes
import Time.SCalendar.Internal
import qualified Data.Set as S ( empty
                               , null
                               , size
                               , difference
                               , isSubsetOf
                               , union
                               , unions   )


-- << Basic calendar constructor: It takes a FirstDay (UTCTime) and the size of the
--    calendar (NumDays) and returns a Calendar whose size is the first power of 2 which
--    is equal or greater that the number of days we want.
createCalendar :: FirstDay -> NumDays -> Maybe Calendar
createCalendar fstDay numDays
  | numDays <= 1 = Nothing
  | otherwise = Just $ go fstDay power
  where
    oneDay = 86400 :: NominalDiffTime
    power = powerOfTwo numDays
    go from factor
      | parentDist == 0 = (TimeUnit from S.empty S.empty)
      | otherwise = Node (from, addUTCTime (oneDay * parentDist) from)
                         S.empty
                         S.empty
                         (go from (factor - 1))
                         (go (addUTCTime (oneDay * childDist) from) (factor - 1))
      where
        parentDist = (2^factor) - 1
        childDist = 2^(factor - 1)

-- << This is like createCalendar, but this function attaches a set of Identifiers
--    to the Calendar.
createSCalendar :: FirstDay -> NumDays -> Set Text -> Maybe SCalendar
createSCalendar _ _ tUnits
  | null tUnits = Nothing
createSCalendar fstDay numDays tUnits = do
  calendar <- createCalendar fstDay numDays
  return $ SCalendar tUnits calendar

-- << Given a calendar of size 2^n, this function augments that calendar k times,
--    that is, 2^(n+k). The new calendar is  properly updated.
augmentCalendar :: SCalendar -> Int -> Maybe SCalendar
augmentCalendar _ k
  | k <= 0 = Nothing
augmentCalendar (SCalendar _ (TimeUnit _ _ _)) _ = Nothing
augmentCalendar (SCalendar _ (Empty _)) _  = Nothing
augmentCalendar (SCalendar totalUnits calendar) k = do
  let (from, to) = getInterval calendar
      newSize = (calendarSize calendar) * (2^k)
  -- create a bigger calendar with a space for our smaller calendar
  largerCal <- createCalendar from newSize
  (slot, bs) <- goToNode (from, to) largerCal
  -- put the smaller calendar in the slot and update the larger calendar
  updatedCal <- updateQ (calendar, bs)
  (root, []) <- upToRoot updatedCal
  return $ SCalendar totalUnits root

-- <<  Given an interval, an amount of units to be reserved, the number of
--     available units and a calendar this function determines if that period of time
--     and quantity are available in that calendar.
isQuantityAvailable :: Quantity
                    -> (From, To)
                    -> SCalendar
                    -> Bool
isQuantityAvailable quant interval (SCalendar totalUnits _)
  | S.null totalUnits = False
  | quant <= 0 = False
  | quant > S.size totalUnits = False
isQuantityAvailable quant interval (SCalendar totalUnits calendar) =
  let result = do
        maybeBarrier <- intervalFitsCalendar interval calendar
        checkAv interval quant totalUnits (calendar, [])
  in if result == Nothing || result == Just [] then False else True
  where
    barrier bool = if bool == False then  Nothing else Just ()
    checkAv :: (From, To)
            -> Quantity
            -> TotalUnits
            -> CalendarZipper -> Maybe [()]
    checkAv _ _ _ (Empty _, _) = Nothing -- A period that includes an empty leaf is not available
    checkAv _ qt units (TimeUnit t q qn, bs) = do
      qMax <- getQMax (TimeUnit t q qn, bs)
      let avUnits = S.size (S.difference units qMax)
      if qt <= avUnits then Just [()] else Nothing
    checkAv (lower,upper) qt units node = do
      let (Node (from, to) q qn left right, bs) = node
      qMax <- getQMax node
      let avUnits = S.size (S.difference units qMax)
      -- Propagate a Nothing if conditions are not met
      maybeBarrier <- barrier $ qt <= avUnits || (not $ isIncluded (from, to) (lower, upper))
      (lChild, bsl) <- goLeft node
      (rChild, bsr) <- goRight node
      let (fromL, toL) = getInterval lChild
          (fromR, toR) = getInterval rChild
      lAnswer <- if lower >= fromR
                 then Just []
                 else checkAv (lower,upper) qt units (lChild, bsl)
      rAnswer <- if upper <= toL
                 then Just []
                 else checkAv (lower,upper) qt units (rChild, bsr)
      return $ concat [lAnswer, rAnswer]

-- << Given a Reservation, and a SCalendar this function determines if that reservation is
--    available in that calendar.
isReservAvailable :: Reservation
                  -> SCalendar
                  -> Bool
isReservAvailable (Reservation resUnits _) (SCalendar totalUnits _)
  | S.null totalUnits = False
  | not $ S.isSubsetOf resUnits totalUnits = False
isReservAvailable (Reservation resUnits interval) (SCalendar totalUnits calendar) =
  let result = do
        maybeBarrier <- intervalFitsCalendar interval calendar
        checkAv (Reservation resUnits interval) totalUnits (calendar, [])
  in if result == Nothing || result == Just [] then False else True
  where
    barrier bool = if bool == False then Just () else Nothing
    checkAv :: Reservation
            -> TotalUnits
            -> CalendarZipper -> Maybe [()]
    checkAv _ _ (Empty _, _) = Nothing -- A period that includes an empty leaf is not available.
    checkAv (Reservation rUnits _) units (TimeUnit t q qn, bs) = do
      qMax <- getQMax (TimeUnit t q qn, bs)
      let avUnits = S.difference units qMax
          isSubset =  S.isSubsetOf rUnits avUnits
      if  isSubset then Just [()] else Nothing
    checkAv (Reservation rUnits (lower, upper)) units node = do
      qMax <- getQMax node
      let (Node (from, to) q qn left right, bs) = node
          avUnits = S.difference units qMax
          isSubset =  S.isSubsetOf rUnits avUnits
      -- If rUnits is not a subset of avUnits and (from, to) is included in (lower, upper),
      -- then there's no availability. Thus propagate a Nothing
      maybeBarrier <-  barrier $ (not isSubset) && (isIncluded (from, to) (lower, upper))
      (lChild, bsl) <- goLeft node
      (rChild, bsr) <- goRight node
      let (fromL, toL) = getInterval lChild
          (fromR, toR) = getInterval rChild
      lAnswer <- if lower >= fromR
                 then Just []
                 else checkAv (Reservation rUnits (lower, upper)) units (lChild, bsl)
      rAnswer <- if upper <= toL
                 then Just []
                 else checkAv (Reservation rUnits (lower, upper)) units (rChild, bsr)
      return $ concat [lAnswer, rAnswer]

-- << This function inserts reservations into a calendar without any constraint. This function
--    is useful if you want to insert reservations which are not included in the current
--    TotalUnits of an SCalendar.
reservePeriod_ :: Reservation
               -> Calendar
               -> Maybe Calendar
reservePeriod_ (Reservation  set (cIn, cOut)) calendar = do
  tmNodes <- topMostNodes (cIn, cOut) calendar
  let tmIntervals = fmap getZipInterval tmNodes
  updatedCalendar <- updateCalendar tmIntervals set calendar (\x y -> Just $ S.union x y)
  return updatedCalendar

-- << This is like reservePeriod_ but reserves many periods at once.
reserveManyPeriods_ :: [Reservation]
                    -> Calendar
                    -> Maybe Calendar
reserveManyPeriods_ [] calendar = Just calendar
reserveManyPeriods_ (reservation:rs) calendar = do
  updatedCalendar <- makeReservation reservation calendar
  reserveManyPeriods_ rs updatedCalendar
  where
    makeReservation res cal
      | maybeCalendar == Nothing = Just cal
      | otherwise = maybeCalendar
      where maybeCalendar = reservePeriod_ res cal

-- << Given a period of time, a set of units to be reserved, and a SCalendar
--    this function returns a new Calendar with a a reservation over that period of
--    time if it is available. The SCalendar returned by this function is a root Node.
reservePeriod :: Reservation
              -> SCalendar
              -> Maybe SCalendar
reservePeriod reservation uCalendar
  | not $ isReservAvailable reservation uCalendar = Nothing
reservePeriod reservation (SCalendar totalUnits calendar) = do
  updatedCalendar <- reservePeriod_ reservation calendar
  return $ SCalendar totalUnits updatedCalendar

-- <<  This function is like reservePeriod, but instead of making one reservation at a time,
--     it takes a list of reservations. This function will return a calendar only with the ones
--     that pass the isReservAvailable test. Take into account that reservations will be inserted
--     in the tree in the order they are in the input list. So, if a reservation conflicts with the
--     ones that have been alredy inserted, it will not be included in the tree.
reserveManyPeriods :: [Reservation]
                   -> SCalendar
                   -> Maybe SCalendar
reserveManyPeriods [] calendar = Just calendar
reserveManyPeriods (reservation:rs) calendar = do
  updatedCalendar <- makeReservation reservation calendar
  reserveManyPeriods rs updatedCalendar
  where
    makeReservation res uCal
      | maybeCalendar == Nothing = Just uCal
      | otherwise = maybeCalendar
      where maybeCalendar = reservePeriod res uCal

-- << This operation takes a Cancellation and returns a new calendar with that Cancellation
--    subtracted from the top-nodes of that Cancellation (Q is therefore updated all over the tree).
--    Be careful with this operation: Two reservations might have the same top nodes, so you
--    must have a way to keep track which elements belong to one reservation and to the other one.
--    deletion in your data base.
--    Note that deleting units from a tree does not prevent you from deleting from a reservation
--    that has never been made. For example, if you have previously reserved n units for (2,7), that
--    reservation will be affected if you delete from a period of time like (2,5). That's why whenever you
--    subtract units from a tree, you must be certain that the period of time has been previously reserved.
--    Also, note that you cannot delete more units than QN, that is, if
--    (size unitsToDelete) > (size QN(node)), a Nothing will be propagated.
cancelPeriod :: Cancellation
             -> Calendar
             -> Maybe Calendar
cancelPeriod (Cancellation  set (cIn, cOut)) calendar = do
  -- To delete from  a previous reservation, we must know its top-nodes.
  tmNodes <- topMostNodes (cIn, cOut) calendar
  let tmIntervals = fmap getZipInterval tmNodes
  cancellation <- updateCalendar tmIntervals set calendar diff
  return cancellation
  where
    diff x y
      | not $ S.isSubsetOf y x = Nothing
      | otherwise = Just ( S.difference x y)

-- << This is like cancelPeriod but cancels many periods at once.
cancelManyPeriods :: [Cancellation]
                  -> Calendar
                  -> Maybe Calendar
cancelManyPeriods [] calendar = Just calendar
cancelManyPeriods (cancellation:cs) calendar = do
  updatedCalendar <- makeCancellation cancellation calendar
  cancelManyPeriods cs updatedCalendar
  where
    makeCancellation canc cal
      | maybeCalendar == Nothing = Just cal
      | otherwise = maybeCalendar
      where maybeCalendar = cancelPeriod canc cal

-- <<  Given a period of time and a Calendar, this function returns a Report which
--     summarizes important data about that period of time.
periodReport :: (From, To) -> SCalendar -> Maybe Report
periodReport period (SCalendar totalUnits calendar) = do
  maybeBarrier <- intervalFitsCalendar period calendar
  tmNodes <- topMostNodes period calendar
  qMaxs <- mapM getQMax tmNodes
  let sQMax =  S.unions qMaxs
  return $ Report period totalUnits sQMax (S.difference totalUnits sQMax)
