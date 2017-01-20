module SCalendar.BreadCrumbs where

import Control.Monad
import SCalendar.DataTypes
import qualified Data.Set as S
import qualified Data.Time as TM
import qualified Data.Text as T


-- BreadCrumbs to move around the calendar -- 

data Crumb = LeftCrumb (From, To) Q QN Calendar
           | RightCrumb (From, To) Q QN Calendar
           deriving Eq


instance Show Crumb where
  show c = "crumb"


type Breadcrumbs = [Crumb]



type CalendarZipper = (Calendar, Breadcrumbs)


                         
goLeft :: CalendarZipper -> Maybe CalendarZipper
goLeft (Node (from, to) q qn left right, bs) =
  Just (left, LeftCrumb (from, to) q qn right : bs)
goLeft (TimeUnit _ _ _, _) = Nothing 
goLeft (Empty _, _) = Nothing



goRight :: CalendarZipper -> Maybe CalendarZipper
goRight(Node (from, to) q qn left right, bs) =
  Just (right, RightCrumb (from, to) q qn left : bs)
goRight (TimeUnit _ _ _, _) = Nothing
goRight (Empty _, _) = Nothing



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


