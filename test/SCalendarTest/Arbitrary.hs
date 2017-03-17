module SCalendarTest.Arbitrary where


import Test.QuickCheck.Arbitrary
import Time.SCalendar.Operations (createCalendar)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import qualified Data.Set as S (empty)
import Time.SCalendar.DataTypes (Calendar(..))
import Test.QuickCheck.Gen (choose)


data Interval = Interval UTCTime UTCTime

startDay = UTCTime (fromGregorian 1970 1 1) 0


-- << Arbitrary size calendars
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
