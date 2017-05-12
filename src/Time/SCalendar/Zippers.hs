module Time.SCalendar.Zippers
  ( CalendarZipper
  , goUp
  , goLeft
  , goRight
  , upToRoot
  , toZipper
  ) where


import Data.Set (Set)
import Data.Text (Text)
import Time.SCalendar.Types (Calendar(..), TimePeriod)


-- | Zippers to move around the calendar | --

data Crumb = LeftCrumb TimePeriod (Set Text) (Set Text) Calendar
           | RightCrumb TimePeriod (Set Text) (Set Text) Calendar
           deriving Eq

instance Show Crumb where
  show LeftCrumb{} = "LeftCrumb"
  show RightCrumb{} = "RightCrumb"


type Breadcrumbs = [Crumb]
type CalendarZipper = (Calendar, Breadcrumbs)


goLeft :: CalendarZipper -> Maybe CalendarZipper
goLeft (Node interval q qn left right, bs) =
  Just (left, LeftCrumb interval q qn right : bs)
goLeft (Unit{}, _) = Nothing

goRight :: CalendarZipper -> Maybe CalendarZipper
goRight(Node interval q qn left right, bs) =
  Just (right, RightCrumb interval q qn left : bs)
goRight (Unit{}, _) = Nothing

goUp :: CalendarZipper -> Maybe CalendarZipper
goUp (calendar, LeftCrumb interval q qn right : bs)
  = Just (Node interval q qn calendar right, bs)
goUp (calendar, RightCrumb interval q qn left : bs)
  = Just (Node interval q qn left calendar, bs)
goUp (_, []) = Nothing

upToRoot :: CalendarZipper -> Maybe CalendarZipper
upToRoot (node, []) = Just (node, [])
upToRoot zipper = do
  parent <- goUp zipper
  upToRoot parent

toZipper :: Calendar -> CalendarZipper
toZipper calendar = (calendar, [])
