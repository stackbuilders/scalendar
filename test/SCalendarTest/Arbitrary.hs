module SCalendarTest.Arbitrary where


import Test.QuickCheck.Arbitrary
import Time.SCalendar.Operations (createCalendar)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import qualified Data.Set as S (empty)
import Time.SCalendar.DataTypes (Calendar(..))
import Test.QuickCheck.Gen (choose)


-- << Arbitrary size calendars
instance Arbitrary Calendar where
  arbitrary = do
    -- Random size calendars up to 512 days
    -- and starting from 1970
    size <- choose (2, 9)
    let startDay = UTCTime (fromGregorian 1970 1 1) 0
    maybe (return $ TimeUnit startDay S.empty S.empty) return (createCalendar startDay size)
