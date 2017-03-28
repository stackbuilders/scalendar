module SCalendarTest.Arbitrary where


import Test.QuickCheck.Arbitrary
import SCalendarTest.Helpers (testIdentifiers)
import Control.Monad (replicateM)
import Time.SCalendar.Operations (createCalendar)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T (Text, pack)
import qualified Data.Set as S (Set, empty, fromList, toList)
import Time.SCalendar.DataTypes (Calendar(..), Reservation (..))
import Time.SCalendar.Zippers ( CalendarZipper(..)
                              , goRight
                              , goLeft           )
import Time.SCalendar.Internal ( getZipInterval
                               , getInterval
                               , calendarSize
                               , intervalFitsCalendar  )
import Test.QuickCheck.Gen (Gen, choose, sized, vectorOf)


-- | Convinient type for Intervals which fit a Calendar
data CalendarReservations = CalReservs Calendar [Reservation]

data Interval = Interval UTCTime UTCTime

newtype Identifier = Identifier Text
  deriving (Eq, Ord)

newtype Identifiers = Identifiers (Set Identifier)

data RandomZippers = RandomZippers CalendarZipper CalendarZipper


-- | Random calendars will start from 1970/1/1
startDay = UTCTime (fromGregorian 1970 1 1) 0


-- | Arbitrary-size calendars
instance Arbitrary Calendar where
  arbitrary = do
    -- ^ Random size calendars up to 512 days
    --   and starting from 1970
    size <- choose (2, 9)
    maybe (return $ TimeUnit startDay S.empty S.empty) return (createCalendar startDay size)


-- | Arbitrary instance for time Intervals
instance Arbitrary Interval where
  arbitrary = do
    month <- choose (1, 12)
    day <- choose (1, 27)
    let to = UTCTime (fromGregorian 1970 month day) 0
    return $ Interval startDay to

instance Show Interval where
  show (Interval from to) = show (from, to)
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
      buildReserv from to = sized $ \n -> do
        k <- choose (1, n)
        identifiers <- vectorOf k arbitrary
        return $ Reservation (S.fromList $ (\(Identifier t) -> t) <$> identifiers) (from, to)
      -- ^ --
      getSuitableInterval cal = do
        interval@(Interval from to) <- arbitrary
        maybe (getSuitableInterval cal)
              (const $ buildReserv from to)
              (intervalFitsCalendar (from, to) cal)

instance Show CalendarReservations where
  show (CalReservs calendar reservs) =
    "Calendar root: " ++ (show $ getInterval calendar) ++
    " , " ++
    "Reservations: " ++ (show reservs)
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
    return $ maybe (RandomZippers root root) id maybeZippers
    where
      getDepth :: Calendar -> Int
      getDepth cal = round $
        logBase 2 (fromIntegral $ calendarSize cal)
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
-- | --
