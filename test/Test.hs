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
