module SCalendarTest.Arbitrary where


import Test.QuickCheck.Arbitrary
import SCalendarTest.Helpers (testIdentifiers, startDay)
import Control.Monad (replicateM, guard)
import Data.Set (Set)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (Text)
import qualified Data.Set as S (fromList)
import Time.SCalendar.Types ( Calendar(..)
                            , Reservation
                            , TimePeriod
                            , createCalendar
                            , makeTimePeriod
                            , makeReservation
                            , getFrom
                            , getTo          )
import Time.SCalendar.Zippers ( CalendarZipper
                              , goRight
                              , goLeft           )
import Time.SCalendar.Internal ( getZipInterval
                               , getInterval
                               , daysBetween
                               , intervalFitsCalendar  )
import Test.QuickCheck.Gen (choose, sized, vectorOf)


-- | Convinient type for Intervals which fit a Calendar
data CalendarReservations = CalReservs Calendar [Reservation]

newtype Identifier = Identifier Text
  deriving (Eq, Ord)

newtype Identifiers = Identifiers (Set Identifier)

data RandomZippers = RandomZippers CalendarZipper CalendarZipper


-- | Arbitrary-size calendars
instance Arbitrary Calendar where
  arbitrary = do
    let (year, month, day) = startDay
    -- ^ Random size calendars up to 512 days
    --   and starting from 1970
    size <- choose (2, 9)
    return $ fromJust $ createCalendar year month day size

-- | Arbitrary instance for time Intervals
instance Arbitrary TimePeriod where
  arbitrary = do
    numDays <- choose (1, 27)
    return $ fromJust $ makeTimePeriod 1970 1 1 numDays
-- | --


-- | Arbitrary instance for a single Identifier
instance Arbitrary Identifier where
  arbitrary = do
    i <- choose (0, 99)
    return $ Identifier $ testIdentifiers !! i

instance Show Identifier where
  show (Identifier i) = show i
-- | --


-- | Arbitrary instance for CalendarReservations
instance Arbitrary CalendarReservations where
  arbitrary = do
    calendar <- arbitrary
    n <- choose (1, 50)
    reservs <- replicateM n $ getSuitableInterval calendar
    return $ CalReservs calendar reservs
    where
      buildReserv interval = sized $ \n -> do
        k <- choose (1, abs $ n + 1)
        identifiers <- vectorOf k arbitrary
        return $ fromJust $ makeReservation interval (S.fromList $ (\(Identifier t) -> t) <$> identifiers)
      -- ^ --
      getSuitableInterval cal = do
        interval <- arbitrary
        maybe (getSuitableInterval cal)
              (const $ buildReserv interval)
              (guard $ intervalFitsCalendar interval cal)

instance Show CalendarReservations where
  show (CalReservs calendar reservs) =
    "Calendar root: " ++ show (getInterval calendar) ++
    " , " ++
    "Reservations: " ++ show reservs
-- | --


-- | Arbitrary pair of Zippers belonging to the same calendar
instance Arbitrary RandomZippers where
  arbitrary = do
    calendar <- arbitrary
    let depth = getDepth calendar
    zip1Depth <- choose (0, depth)
    zip2Depth <- choose (0, depth)
    let root = (calendar, [])
        maybeZippers = do
          zip1 <- goDown (Just root) zip1Depth
          zip2 <- goDown (Just root) zip2Depth
          return $ RandomZippers zip1 zip2
    return $ fromMaybe (RandomZippers root root) maybeZippers
    where
      getDepth :: Calendar -> Int
      getDepth cal = round $
        let interval = getInterval cal
        in logBase 2 (fromIntegral $ daysBetween (getFrom interval) (getTo interval))
      -- ^ --
      pickBranch zipper n
        | n `mod` 2 == 0 = goRight zipper
        | otherwise = goLeft zipper
      -- ^ --
      goDown maybeZipper 0 = maybeZipper
      goDown maybeZipper i =
        maybeZipper >>= (\zipper ->  goDown (pickBranch zipper i) (i-1))

instance Show RandomZippers where
  show (RandomZippers zip1 zip2) = show (getZipInterval zip1, getZipInterval zip2)
