module Main where


import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property (property, mapSize)
import SCalendarTest.Arbitrary
import SCalendarTest.Internal ( alwaysGreateOrEqualThanN
                              , eqIntervalsIfIncludeEachOther
                              , returnsTargetZipper
                              , isLeftMostTopNode
                              , isRightMostTopNode
                              , returnsCommonParent
                              , leftMostAndRightMostInTopMost
                              , outerMostNodesIncludeIntermediate
                              , ifOnlyOneTopNodeItEqualsInterval
                              , parentOfTopNodesNotIncluded     )
import SCalendarTest.Operations ( calendarSizePowerOfTwo
                                , symmetricalIntervalLength
                                , qMaxOfParentIncludedInChildren
                                , quantityNotAvailableAfterReservation
                                , periodNotAvailableAfterReservation
                                , reservAvailableAfterCancellation  )


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
    it "creates a calendar with symmetric intervals" $ do
      property symmetricalIntervalLength
  describe "goToNode :: (From, To) -> Calendar -> Maybe CalendarZipper" $ do
    it "goes to the node with interval (From, To) in the calendar" $ do
      property returnsTargetZipper
  describe "leftMostTopNode :: (From, To) -> Calendar -> Maybe CalendarZipper" $ do
    it "returns a Zipper with a valid left-most interval" $ do
      property isLeftMostTopNode
  describe "isRightMostTopNode :: Interval -> Calendar -> Bool" $ do
    it "returns a Zipper with a valid right-most interval" $ do
      property isRightMostTopNode
  describe "commonParent :: CalendarZipper -> CalendarZipper -> Maybe CalendarZipper" $ do
    it "returns a Zipper which is a common parent node of its arguments" $ do
      property returnsCommonParent
  describe "topMostNodes :: (From, To) -> Calendar -> Maybe [CalendarZipper]" $ do
    it "returns a list of topmost-nodes *including* the rightmost and the leftmost ones" $ do
      property leftMostAndRightMostInTopMost
    it "returns a list of topmost-nodes *included* in the rightmost and the leftmost ones" $ do
      property outerMostNodesIncludeIntermediate
    it "returns a list of topmost-nodes with no parent included in (From, To)" $ do
      property parentOfTopNodesNotIncluded
    context "when there is only one topmost-node" $ do
      it "must return an interval equal to (From, To)" $ do
        property ifOnlyOneTopNodeItEqualsInterval
  describe "reserveManyPeriods :: [Reservation] -> SCalendar -> Maybe SCalendar" $ do
    it "returns a Calendar which satisfies that QMax of parent node is included in QMax of left child" $ do
      property qMaxOfParentIncludedInChildren
  describe "isQuantityAvailable :: Quantity -> (From, To) -> SCalendar -> Bool" $ do
    it "determines if a quantity is available after a reservation" $ do
      property quantityNotAvailableAfterReservation
  describe "isReservAvailable :: Reservation -> SCalendar -> Bool" $ do
    context "when a node has already been reserved" $ do
      it "returns false for the same reservation in that node" $ do
        property periodNotAvailableAfterReservation
  describe "cancelManyPeriods :: [Cancellation] -> Calendar -> Maybe Calendar" $ do
    context "when a reservation in a node is cancelled" $ do
      it "becomes again availabale" $ do
        property reservAvailableAfterCancellation
