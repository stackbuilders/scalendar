module SCalendarTest.Arbitrary where


import Test.QuickCheck.Arbitrary
import Time.SCalendar.Operations (createCalendar)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import qualified Data.Set as S (empty)
import Time.SCalendar.DataTypes (Calendar(..))
import Time.SCalendar.Zippers ( CalendarZipper(..)
                              , goRight
                              , goLeft           )
import Time.SCalendar.Internal ( getZipInterval
                               , getInterval
                               , calendarSize
                               , intervalFitsCalendar  )
import Test.QuickCheck.Gen (choose)


-- << Convinient type for an Interval which fit a Calendar
data CalendarInterval = CalInterval Calendar Interval

data Interval = Interval UTCTime UTCTime

data RandomZippers = RandomZippers CalendarZipper CalendarZipper

startDay = UTCTime (fromGregorian 1970 1 1) 0


-- << Arbitrary-size calendars
instance Arbitrary Calendar where
  arbitrary = do
    -- Random size calendars up to 512 days
    -- and starting from 1970
    size <- choose (2, 9)
    maybe (return $ TimeUnit startDay S.empty S.empty) return (createCalendar startDay size)


-- << Arbitrary instance for time Intervals
instance Arbitrary Interval where
  arbitrary = do
    month <- choose (1, 12)
    day <- choose (1, 27)
    let to = UTCTime (fromGregorian 1970 month day) 0
    return $ Interval startDay to

instance Show Interval where
  show (Interval from to) = show (from, to)
-- >>

-- << Arbitrary instance for CalendarInterval
instance Arbitrary CalendarInterval where
  arbitrary = do
    calendar <- arbitrary
    getSuitableInterval calendar
    where
      getSuitableInterval cal = do
        interval@(Interval from to) <- arbitrary
        maybe (getSuitableInterval cal)
              (const $ return $ CalInterval cal interval)
              (intervalFitsCalendar (from, to) cal)

instance Show CalendarInterval where
  show (CalInterval calendar (Interval from to)) =
    "Calendar root: " ++ (show $ getInterval calendar) ++
    " , " ++
    "Interval: " ++  (show (from, to))
-- >>


-- << Arbitrary pair of Zippers belonging to the same calendar
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
      -- << >> --
      pickBranch zipper n
        | n `mod` 2 == 0 = goRight zipper
        | otherwise = goLeft zipper
      -- << >> --
      goDown maybeZipper 0 = maybeZipper
      goDown maybeZipper i =
        maybeZipper >>= (\zipper ->  goDown (pickBranch zipper i) (i-1))

instance Show RandomZippers where
  show (RandomZippers zip1 zip2) = show (getZipInterval zip1, getZipInterval zip2)
-- >>
