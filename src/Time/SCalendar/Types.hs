module Time.SCalendar.Types
  ( TimePeriod(..)
  , Reservation(..)
  , Cancellation(..)
  , SCalendar(..)
  , Calendar(..)
  , Report(..)
  , isIncluded
  , getFrom
  , getTo
  , toTimeUnit
  , makeTimePeriod
  , makeReservation
  , makeCancellation
  , createCalendar
  , createSCalendar
  , oneDay
  , powerOfTwo
  ) where


import Data.Time ( UTCTime(..)
                 , NominalDiffTime
                 , addUTCTime
                 , fromGregorianValid )
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as S (empty)


-- | This data type is either a TimeInterval of the form (start-date, end-date)
-- or a TimeUnit which, in this case, is a nominal day. The time unit of this calendar
-- library is a nominal day, that is, 86400 seconds. TimeIntervals as well as
-- TimeUnits are stored as UTCTime so that it is easy to transform results to local
-- time or store results in databases as timestamps.
data TimePeriod =
    TimeInterval UTCTime UTCTime -- ^ TimeIntervals represent the number of days that a node
                                 -- in a calendar covers from a start-date up to an end-date.
  | TimeUnit UTCTime -- ^ TimeUnits are only encountered in the leaves of a calendar and represent
                     -- a specific day of the calendar.
  deriving (Eq, Show)

-- | Check if a time-period `t1` is included in a time-period `t2`. Note that neither a
-- TimeUnit can be included in another TimeUnit nor a TimeInterval can be included
-- in a TimeUnit. If two TimeIntervals are equal they are said to be included in
-- one another.
isIncluded :: TimePeriod -> TimePeriod -> Bool
isIncluded (TimeUnit _) (TimeUnit _) = False
isIncluded (TimeUnit t) (TimeInterval from to) = from <= t && t <= to
isIncluded (TimeInterval _ _) (TimeUnit _)  = False
isIncluded (TimeInterval ifrom ito)  (TimeInterval ofrom oto)
  = and [ ofrom <= ifrom, ifrom <= oto, ofrom <= ito, ito <= oto, ifrom <= ito ]

-- | Getter function to get the UTCTime start-date from a TimePeriod. For a TimeUnit
-- the start-sate and the end-date are equal.
getFrom :: TimePeriod -> UTCTime
getFrom (TimeUnit t) = t
getFrom (TimeInterval from _) = from

-- | Getter function to fet the UTCTime end-date from a TimePeriod. Again, for a TimeUnit
-- the start-sate and the end-date are equal.
getTo :: TimePeriod -> UTCTime
getTo (TimeUnit t) = t
getTo (TimeInterval _ to) = to

-- | This function transforms a TimeInterval into a TimeUnit in case that the start-date and
-- end-date of that TimeInterval are equal.
toTimeUnit :: TimePeriod -> TimePeriod
toTimeUnit i@(TimeUnit _) = i
toTimeUnit i@(TimeInterval t1 t2)
  | t1 == t2 = TimeUnit t1
  | otherwise = i

-- | A Reservation is the product of a set of identifiers and a TimePeriod over which the
-- resources identified by the set will be reserved.
data Reservation = Reservation
  { reservUnits :: Set Text -- ^ Set of identifiers which point to reservable resources.
  , reservPeriod :: TimePeriod -- ^ TimePeriod over which the resources will be reserved.
  } deriving (Eq, Show)

-- | A Cancellation is the product of a set of identifiers which point to resources previously
-- reserved in a Calendar and a TimePeriod over which those resources were reserved.
data Cancellation = Cancellation
  { cancUnits :: Set Text -- ^ Set of identifiers which point to resources to be cancelled.
  , cancPeriod :: TimePeriod -- ^ TimePeriod over which the resources will be cancelled.
  } deriving (Eq, Show)

-- | A Report represents a summary of important facts related to an SCalendar.
data Report = Report
  { reportPeriod :: TimePeriod -- ^ The TimePeriod which the report covers.
  , totalUnits :: Set Text -- ^ The set of total identifiers reservable in the SCalendar this Report belongs to.
  , reservedUnits :: Set Text -- ^ The set of total identifiers which have been reserved in a TimePeriod in
                              -- the SCalendar related to this Report.
  , remainingUnits :: Set Text -- ^ The set of total identifiers which are still available in a Time<Period in the
                               -- SCalendar related to this Report.
  } deriving (Eq, Show)

-- | A Calendar is a recursive tree-structure whose nodes are TimePeriods representing the interval
-- of time covered by them. TimeUnits are only encountered in the leaves since they represent
-- specific days, or time units, of the Calendar. The unit of time of this Calendar library is a nominal
-- day (or 86400 seconds). Each node of a Calendar also carries additional data according to the
-- "top-nodes" algorithm: a `Q` set and a `QN` set. For more information about the meaning of these
-- sets visit: <https://en.wikipedia.org/wiki/Top-nodes_algorithm>
data Calendar =
    Unit TimePeriod (Set Text) (Set Text)
  | Node TimePeriod (Set Text) (Set Text) Calendar Calendar
  deriving (Eq, Show)

-- | An SCalendar is the product of a set of identifiers, which point to a set of available resources,
-- and a Calendar.
data SCalendar = SCalendar
  { calUnits :: Set Text -- ^ Set of resources which can be reserved for the TimePeriod covered by
                         -- the root node of the Calendar.
  , calendar :: Calendar -- ^ Calendar which covers the complete period of time over which a set of
                         -- resources can be reserved.
  } deriving (Eq, Show)


-- | Given a year, a month and a day this function creates a time period which covers the specified
-- number of days.
makeTimePeriod :: Integer -- ^ Year.
               -> Int -- ^ Month.
               -> Int -- ^ Day.
               -> Int -- ^ Number of days covered by TimePeriod.
               -> Maybe TimePeriod
makeTimePeriod _ _ _ numDays
  | numDays < 0 = Nothing
makeTimePeriod year month day numDays = do
  gregDay <- fromGregorianValid year month day
  let from = UTCTime gregDay 0
      to = (fromIntegral numDays * oneDay) `addUTCTime` from
  return $ if numDays == 0
           then TimeUnit from
           else TimeInterval from to

-- | Given a TimePeriod and a set of identifiers this function creates a reservation.
makeReservation :: TimePeriod -- ^ TimePeriod which the rerservation will cover.
                -> Set Text -- ^ Set of identifiers which point to the resources to be reserved from an SCalendar.
                -> Maybe Reservation
makeReservation period units
  | null units = Nothing
  | not $ isValidInterval period' = Nothing
  | otherwise = Just $ Reservation units period'
  where
    period' = toTimeUnit period

-- | Given a TimePeriod and a set of identifiers this function creates a cancellation.
makeCancellation :: TimePeriod -- ^ TimePeriod which the cancellation will cover.
                 -> Set Text -- ^ Set of identifiers which point to the resources to be cancelled from an SCalendar.
                 -> Maybe Cancellation
makeCancellation period units
  | null units = Nothing
  | not $ isValidInterval period' = Nothing
  | otherwise = Just $ Cancellation units period'
  where
    period' = toTimeUnit period

-- | Given a year, a month, and a day this function creates a Calendar which covers the specified
-- number of days. The TimePeriod in the root node of a Calendar does not exactly span the
-- number of days specified in the function, but a number of days which is a power of 2 and
-- which is greater than or equal to the number of days specified.
createCalendar :: Integer -- ^ Year.
               -> Int -- ^ Month.
               -> Int -- ^ Day.
               -> Int -- ^ Number of days covered by the Calendar.
               -> Maybe Calendar
createCalendar year month day numDays
  | numDays <= 1 = Nothing
  | otherwise = do
      gregDay <- fromGregorianValid year month day
      let fstDay  = UTCTime gregDay 0
      return $ go fstDay power
  where
    power = powerOfTwo numDays
    go from factor
      | parentDist == 0 = Unit (TimeUnit from) S.empty S.empty
      | otherwise =
          Node (TimeInterval from ((oneDay * parentDist) `addUTCTime` from))
               S.empty
               S.empty
               (go from (factor - 1))
               (go ((oneDay * childDist) `addUTCTime` from) (factor - 1))
      where
        parentDist = (2^factor) - 1
        childDist = 2^(factor - 1)

-- | This constructor additionally attaches a set of identifiers, which point to the available resources of the
-- calendar. Thus, this function creates an SCalendar which is basically a Calendar with a set of resources which
-- can be reserved over the period of time determined by the root node of the Calendar.
createSCalendar :: Integer -- ^ Year.
                -> Int -- ^ Month.
                -> Int -- ^ Day.
                -> Int -- ^ Number of days covered by the Calendar.
                -> Set Text -- ^ Set of resources which can be reserved for the TimePeriod covered by
                            -- the root node of the Calendar.
                -> Maybe SCalendar
createSCalendar _ _ _ _ tUnits
  | null tUnits = Nothing
createSCalendar year month day numDays tUnits = do
  calendar <- createCalendar year month day numDays
  return $ SCalendar tUnits calendar


-- HELPER FUNCTIONS
isValidInterval :: TimePeriod -> Bool
isValidInterval (TimeUnit _) = True
isValidInterval (TimeInterval from to) = from < to

powerOfTwo :: Int -> Int
powerOfTwo n =
  let power = ceiling $ logBase 2 (fromIntegral $ abs n)
  in if power > 1 then power else 2

oneDay :: NominalDiffTime
oneDay = 86400
