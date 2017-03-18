module SCalendarTest.Operations where


import Time.SCalendar.Operations (createCalendar)
import SCalendarTest.Helpers (getUTCdayNum)
import Time.SCalendar.Internal ( calendarSize
                               , powerOfTwo
                               , getZipInterval )
import Time.SCalendar.Zippers (goLeft, goRight)
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Clock (UTCTime(..))
import Time.SCalendar.DataTypes (Calendar(..))
import qualified Data.Set as S (empty)


calendarSizePowerOfTwo :: Int -> Bool
calendarSizePowerOfTwo n =
  let size = if n > 1 then n else (abs n) + 2
      startDay = UTCTime (fromGregorian 1970 1 1) 0
      calendar = maybe (TimeUnit startDay S.empty S.empty)
                       id
                       (createCalendar startDay size)
  in calendarSize calendar == 2 ^ (powerOfTwo size)

symmetricalIntervalLength :: Calendar -> Bool
symmetricalIntervalLength calendar =
  maybe False id (checkSimmetry calZipper)
  where
    calZipper = (calendar, [])
    -- << >> --
    checkSimmetry ((TimeUnit _ _ _), _) = Just True
    checkSimmetry (Empty _, _) = Just True
    checkSimmetry zipper = do
      leftChild <- goLeft zipper
      rightChild <- goRight zipper
      let (from1, to1) = getZipInterval leftChild
          (from2, to2) = getZipInterval rightChild
          intervalSymmetry = (getUTCdayNum to1) - (getUTCdayNum from1)
                             == (getUTCdayNum to2) - (getUTCdayNum from2)
      return $ intervalSymmetry &&
               maybe False id (checkSimmetry leftChild) &&
               maybe False id (checkSimmetry rightChild)
