module Main where


import Test.Hspec
import Test.QuickCheck.Property (property)
import SCalendarTest.Internal ( alwaysGreateOrEqualThanN
                              , eqIntervalsIfIncludeEachOther )


main :: IO ()
main = hspec $ do
  describe "powerOftwo :: Int -> Int" $ do
    it "always returns a power of 2 greater or equal than its argument" $ do
      property alwaysGreateOrEqualThanN
  describe "isIncluded :: Ord a => (a,a) -> (a,a)" $ do
    it "determines if the first interval is included in the second one" $ do
      property eqIntervalsIfIncludeEachOther
