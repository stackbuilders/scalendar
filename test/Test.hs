module Main where


import Test.Hspec
import Test.QuickCheck.Property (property)
import SCalendarTest.Arbitrary
import SCalendarTest.Internal ( alwaysGreateOrEqualThanN
                              , eqIntervalsIfIncludeEachOther
                              , returnsTargetZipper         )
import SCalendarTest.Operations ( calendarSizePowerOfTwo
                                , symmetricalIntervalLength )


main :: IO ()
main = hspec $ do
  describe "powerOftwo :: Int -> Int" $ do
    it "always returns a power of 2 greater or equal than its argument" $ do
      property alwaysGreateOrEqualThanN
  describe "isIncluded :: Ord a => (a,a) -> (a,a)" $ do
    it "determines if the first interval is included in the second one" $ do
      property eqIntervalsIfIncludeEachOther
  describe "createCalendar :: createCalendar :: FirstDay -> NumDays -> Maybe Calendar" $ do
    it "creates a calendar with a number of days 2^(powerOftwo NumDays)" $ do
      property calendarSizePowerOfTwo
  describe "createCalendar :: createCalendar :: FirstDay -> NumDays -> Maybe Calendar" $ do
    it "creates a calendar with symmetric intervals" $ do
      property symmetricalIntervalLength
  describe "goToNode :: (From, To) -> Calendar -> Maybe CalendarZipper" $ do
    it "goes to the node with interval (From, To) in the calendar" $ do
      property returnsTargetZipper
