module SCalendarTest.Helpers ( getUTCdayNum, testIdentifiers  ) where


import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (Gen)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Calendar (toGregorian)
import Data.Text (Text)
import qualified Data.Text as T (Text, pack)


testIdentifiers :: [Text]
testIdentifiers =  T.pack <$> (show <$> [1.. 100])

getUTCdayNum :: UTCTime -> Int
getUTCdayNum (UTCTime day _) =
  let (_, _, num) = toGregorian day
  in num
