module Time.SCalendar.Types
  ( TimePeriod
  , Reservation
  , Cancellation
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
  , reservUnits
  , reservPeriod
  , cancUnits
  , cancPeriod
  ) where


import Data.Time ( UTCTime(..)
                 , NominalDiffTime
                 , addUTCTime
                 , fromGregorianValid )
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as S (empty)


data TimePeriod = TimeInterval UTCTime UTCTime | TimeUnit UTCTime
  deriving (Eq, Show)


isIncluded :: TimePeriod -> TimePeriod -> Bool
isIncluded (TimeUnit _) (TimeUnit _) = False
isIncluded (TimeUnit t) (TimeInterval from to) = from <= t && t <= to
isIncluded (TimeInterval _ _) (TimeUnit _)  = False
isIncluded (TimeInterval ifrom ito)  (TimeInterval ofrom oto)
  = and [ ofrom <= ifrom, ifrom <= oto, ofrom <= ito, ito <= oto, ifrom <= ito ]

getFrom :: TimePeriod -> UTCTime
getFrom (TimeUnit t) = t
getFrom (TimeInterval from _) = from

getTo :: TimePeriod -> UTCTime
getTo (TimeUnit t) = t
getTo (TimeInterval _ to) = to

toTimeUnit :: TimePeriod -> TimePeriod
toTimeUnit i@(TimeUnit _) = i
toTimeUnit i@(TimeInterval t1 t2)
  | t1 == t2 = TimeUnit t1
  | otherwise = i


data Reservation = Reservation
  { reservUnits :: Set Text
  , reservPeriod :: TimePeriod
  } deriving (Eq, Show)

data Cancellation = Cancellation
  { cancUnits :: Set Text
  , cancPeriod :: TimePeriod
  } deriving (Eq, Show)

data Report = Report
  { reportPeriod :: TimePeriod
  , totalUnits :: Set Text
  , reservedUnits :: Set Text
  , remainingUnits :: Set Text
  } deriving (Eq, Show)

data Calendar =
    Unit TimePeriod (Set Text) (Set Text)
  | Node TimePeriod (Set Text) (Set Text) Calendar  Calendar
  deriving (Eq, Show)

-- | An SCalendar is a Set of identifiers togeher with a Calendar
data SCalendar = SCalendar
  { calUnits :: Set Text
  , calendar :: Calendar
  } deriving (Eq, Show)


makeTimePeriod :: Integer -> Int -> Int -> Int -> Maybe TimePeriod
makeTimePeriod _ _ _ numDays
  | numDays < 0 = Nothing
makeTimePeriod year month day numDays = do
  gregDay <- fromGregorianValid year month day
  let from = UTCTime gregDay 0
      to = (fromIntegral numDays * oneDay) `addUTCTime` from
  return $ if numDays == 0
           then TimeUnit from
           else TimeInterval from to

makeReservation :: TimePeriod -> Set Text -> Maybe Reservation
makeReservation period units
  | null units = Nothing
  | not $ isValidInterval period' = Nothing
  | otherwise = Just $ Reservation units period'
  where
    period' = toTimeUnit period

makeCancellation :: TimePeriod -> Set Text -> Maybe Cancellation
makeCancellation period units
  | null units = Nothing
  | not $ isValidInterval period' = Nothing
  | otherwise = Just $ Cancellation units period'
  where
    period' = toTimeUnit period

createCalendar :: Integer -> Int -> Int -> Int -> Maybe Calendar
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

createSCalendar :: Integer -> Int -> Int -> Int -> Set Text -> Maybe SCalendar
createSCalendar _ _ _ _ tUnits
  | null tUnits = Nothing
createSCalendar year month day numDays tUnits = do
  calendar <- createCalendar year month day numDays
  return $ SCalendar tUnits calendar

isValidInterval :: TimePeriod -> Bool
isValidInterval (TimeUnit _) = True
isValidInterval (TimeInterval from to) = from < to

powerOfTwo :: Int -> Int
powerOfTwo n =
  let power = ceiling $ logBase 2 (fromIntegral $ abs n)
  in if power > 1 then power else 2

oneDay :: NominalDiffTime
oneDay = 86400
