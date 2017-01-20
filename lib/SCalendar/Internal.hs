module SCalendar.Internal where

import Control.Monad
import SCalendar.BreadCrumbs
import SCalendar.DataTypes
import qualified Data.Set as S
import qualified Data.Time as TM
import qualified Data.Text as T


-- UTILITY FUNCTIONS --

-- Given a calendar, this function returns the number of days of this calendar.
calendarSize :: Calendar -> Int
calendarSize (Node (from, to ) _ _ _ _)
  = 1 + round (TM.diffUTCTime to from / 86400)  
calendarSize node = 0

-- Safely return the head of a list.
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x


-- Find the first power of 2 that is equal or greater than n
powerOfTwo :: Int -> Int
powerOfTwo n =
  go n 0
  where
    go d i
      | 2^i >= d = i
      | otherwise = go d (i+1)


-- Given an interval,this function determines if it is included in another interval.
isIncluded :: (From, To) -> (From, To) -> Bool
isIncluded (from, to) (from', to') =
  from' <= from && from <= to' && from' <= to && to <= to' && from <= to


{-
  This is a helper function to validate some important facts about an interval (from, to) and a calendar:
  - from <= to
  - A Calendar which is just a TimeUnit is not valid.
  - An Empty Leaf of a Calendar is not valid.
  - If the interval is not included in the period of the root node of a calendar, then that interval
    is not valid.
-}
intervalFitsCalendar :: (From, To)
                     -> Calendar
                     -> Maybe ()
intervalFitsCalendar (from, to) _
  | not (from <= to)  = Nothing 
intervalFitsCalendar _ (TimeUnit _ _ _) = Nothing                 
intervalFitsCalendar _ (Empty _) = Nothing
intervalFitsCalendar interval (Node period _  _  _  _)
  | not $ isIncluded interval period = Nothing
  | otherwise = Just ()



-- Given a node this function returns the interval that that node represents
getInterval :: Calendar -> (From, To)
getInterval (TimeUnit unit _ _) = (unit, unit)
getInterval (Node (from, to) _ _ _ _) = (from, to)
getInterval (Empty (from, to)) = (from, to)


-- Like getInterval, but gets the interval from a zipper.
getZipInterval :: CalendarZipper -> (From, To)
getZipInterval (node, _) = getInterval node



-- Get the Q set from a node.
getQ :: CalendarZipper -> Q 
getQ (Empty _, _) = S.empty 
getQ (TimeUnit _ q _, _) = q
getQ (Node _ q _ _ _, _) = q


-- Get the QN set from a node.
getQN :: CalendarZipper -> QN 
getQN (Empty _, _) = S.empty 
getQN (TimeUnit _ _ qn, _) = qn
getQN (Node _ _  qn _ _, _) = qn


-- Given a node this function returns the QMax For that node.
-- QMax = Q + U(QN of parent nodes up to the root node)
getQMax :: CalendarZipper -> Maybe (S.Set T.Text)
getQMax (node, []) = Just $ getQ (node, []) 
getQMax zipper = do
  parent <- goUp zipper
  go parent (getQ zipper)
  where
    go (node, []) sum = do
      let qn = getQN (node, [])
      return $ S.union sum  qn
    go zipper sum = do
      let qn = getQN zipper
      parent <- goUp zipper
      go parent $ S.union sum  qn

      
-- END OF UTILITY FUNCTIONS --



-- Given a period of time and a calendar, this function finds the leftMost top-node of that interval.
-- an empty leaf is not considered a top-most node.
leftMostTopNode :: (From, To)
                -> Calendar
                -> Maybe CalendarZipper
leftMostTopNode interval calendar = do
  maybeBarrier <- intervalFitsCalendar interval calendar
  result <- ltmNode interval (calendar, [])
  maybeHead result
  where
    ltmNode _ (Empty _, _) = Just []
    ltmNode (lower, upper) (TimeUnit t q qn, bs)
      | lower == t && t <= upper = Just [(TimeUnit t q qn, bs)]
      | otherwise = Just []
    ltmNode (lower,upper) node = do
      (lChild, bsl) <- goLeft node
      (rChild, bsr) <- goRight node
      let (Node (from, to) q qn left right, bs) = node
      case lower == from && to <= upper of
        True -> Just [node]
        False -> do
          let (fromL, toL) = getInterval lChild 
              (fromR, toR) = getInterval rChild
          lAnswer <- if lower <= toL  
                     then ltmNode (lower,upper) (lChild, bsl)
                     else Just []
          rAnswer <- if lower >= fromR
                     then ltmNode (lower,upper) (rChild, bsr)
                     else Just []
          return $ concat [lAnswer, rAnswer]




-- Given a period of time and a calendar, this function finds the rightMost top-node of that interval.
-- An empty leaf is not considered a top-most node.
rightMostTopNode :: (From, To)
                 -> Calendar
                 -> Maybe CalendarZipper                                     
rightMostTopNode interval calendar = do
  maybeBarrier <- intervalFitsCalendar interval calendar
  result <- rtmNode interval (calendar, [])
  maybeHead result
  where
    -- There's no rightmost top-node if it is supposed to be in an empty leaf.
    rtmNode _ (Empty _, _) = Just []
    rtmNode (lower, upper) (TimeUnit t q qn, bs)
      | upper == t && t >= lower = Just [(TimeUnit t q qn, bs)]
      | otherwise = Just []
    rtmNode (lower,upper) node = do
      (lChild, bsl) <- goLeft node
      (rChild, bsr) <- goRight node
      let (Node (from, to) q qn left right, bs) = node
      case upper == to && from >= lower of
        True -> Just [node]
        False -> do
          let (fromL, toL) = getInterval lChild 
              (fromR, toR) = getInterval rChild
          lAnswer <- if upper <= toL
                     then rtmNode (lower,upper) (lChild, bsl)
                     else Just []
          rAnswer <- if upper >= fromR
                     then rtmNode (lower,upper) (rChild, bsr)
                     else Just []
          return $ concat [lAnswer, rAnswer]




-- Given two nodes this function finds the common parent node of those nodes, if it exists.
-- The result is only valid if the two nodes (and their breadcrumbs) come from the same calendar.
commonParent :: CalendarZipper
             -> CalendarZipper
             -> Maybe CalendarZipper
commonParent zipper1 zipper2 = do
  let (node1, bs1) = zipper1
      (node2, bs2) = zipper2
      bs1Length = length bs1
      bs2Length = length bs2
  let interval1 = getInterval node1
      interval2 = getInterval node2
  case bs1Length == bs2Length of
    True -> if interval1 == interval2
            then Just zipper1
            else do
              zipper1' <- goUp zipper1
              zipper2' <- goUp zipper2
              commonParent zipper1' zipper2'
    False ->
      case bs1Length < bs2Length of
        True -> do
          zipper2' <- goUp zipper2
          commonParent zipper1 zipper2'
        False -> do
          zipper1' <- goUp zipper1
          commonParent zipper1' zipper2



-- Given a period of time and a calendar, this function returns the topmost nodes of that period of time
-- in the calendar. Empty leaves are not consider to be top-most nodes.
-- Note that this function returns at least the rightmost top-node in case it is found but the leftmost-top node
-- is not found. This is only for convinience, since having at least the rightmost top-node is useful.

topMostNodes :: (From, To)
                -> Calendar
                -> Maybe [CalendarZipper]
topMostNodes (lower, upper) calendar = do
  rtNode <- rightMostTopNode (lower, upper) calendar
  let (fromR, toR) = getZipInterval rtNode
  case (fromR, toR) == (lower, upper) of
    True -> Just [rtNode]
    False -> do
      let leftMost = leftMostTopNode (lower, upper) calendar
      case leftMost of
        Nothing -> return [rtNode]
        Just ltNode -> do       
          let (fromL, toL) = getZipInterval ltNode
          parent <- commonParent ltNode rtNode
          answer <- goDownTree (lower, upper) (fromL, toL) (fromR, toR) parent
          let tmNodes =  ltNode : rtNode : answer
          return tmNodes
  where
    goDownTree :: (From, To)
               -> (From, To)
               -> (From, To)
               -> CalendarZipper -> Maybe [CalendarZipper]
    goDownTree _ _ _ (Empty _, _) = Just []
    goDownTree period leftMost rightMost (TimeUnit t q qn, bs) = 
      case isIncluded (t,t) period of
        True -> if (t, t) == rightMost || (t, t) == leftMost
                then Just []
                else Just [(TimeUnit t q qn, bs)] 
        False -> Just [] 
    goDownTree period leftMost rightMost node = do
      let (Node (from, to) _ _ _ _, bs) = node
      case isIncluded (from, to) period of
        True -> if (from, to) == rightMost || (from, to) == leftMost
                then Just []
                else Just [node]
        False -> do
          lChild <- goLeft node
          rChild <- goRight node
          lAnswer <- goDownTree period  leftMost rightMost lChild
          rAnswer <- goDownTree period  leftMost rightMost rChild
          return $ concat [lAnswer, rAnswer]




-- This function receives a Node (point in a Calendar) and returns a new Node (up to the root node) with
-- Q updated. That is, this function updates the Q all over the tree. Q = U(Q(leftChild), Q(rightChild), QN)
updateQ :: CalendarZipper -> Maybe CalendarZipper 
updateQ (node, []) = Just (node, [])
updateQ zipper = do
  parent <- goUp zipper
  lChild <- goLeft parent
  rChild <- goRight parent
  let (Node period q qn left right, bs) = parent
      lQ = getQ lChild
      rQ = getQ rChild
  updateQ (Node period (S.unions [lQ, rQ, qn]) qn left right, bs)




-- Given a period of time and a Calendar, this function takes you to the node that represents that period,
-- if it exists.
goToNode :: (From, To) -> Calendar -> Maybe CalendarZipper
goToNode interval calendar = do
  result <- go interval (calendar, [])
  maybeHead result
  where
    go (lower, upper) (Empty (from, to), bs)
      | (lower, upper) == (from, to) = Just [(Empty (from, to), bs)]
      | otherwise = Just []
    go (lower, upper) (TimeUnit t q qn, bs) 
      | (lower, upper) == (t,t) = Just [(TimeUnit t q qn, bs)]
      | otherwise = Just []
    go (lower, upper) node = do
      let (Node (from, to) _ _ _ _, bs) = node
      case (lower, upper) == (from, to) of
        True -> Just [node]
        False -> do      
            (lChild, bsl) <- goLeft node
            (rChild, bsr) <- goRight node          
            let (fromL, toL) = getInterval lChild
                (fromR, toR) = getInterval rChild
            lAnswer <- if lower >= fromR
                       then Just []
                       else go (lower,upper) (lChild, bsl)
            rAnswer <- if upper <=  toL
                       then Just []
                       else go (lower,upper) (rChild, bsr)
            return $ concat [lAnswer, rAnswer]




-- This is a helper function both for reserve period and deleteUnitsFromReservation. This function
-- takes a list of topMostNodes intervals, a set of units to be reserved, a calendar, and an binary operation over
-- sets (union or difference). The binary operation determines the way that each node will be updated: union for
-- reservations and difference for deleting from a previous reservation.
updateCalendar :: [(From, To)]
                   -> S.Set T.Text
                   -> Calendar
                   -> (S.Set T.Text -> S.Set T.Text -> Maybe (S.Set T.Text))
                   -> Maybe Calendar
updateCalendar _ _ (Empty _) _ = Nothing                   
updateCalendar [] _ cal _ = Just cal
updateCalendar (period:ps) elts cal f = do
  updatedRoot <- updateQandQN elts period cal 
  updateCalendar ps elts updatedRoot f 
  where
    update s (Empty period, bs) = do
      updateQ (Empty period, bs) -- make sure to update Q all over the tree.
    update s (TimeUnit unit q qn, bs) = do
      newQ <- f q s
      newQN <- f qn s
      let zipper = (TimeUnit unit newQ newQN, bs) 
      updateQ zipper -- make sure to update Q all over the tree.
    update s (Node period q qn left right, bs) = do
      newQ <- f q s
      newQN <- f qn s
      let zipper = (Node period newQ newQN left right, bs)
      updateQ zipper -- make sure to update Q all over the tree.
    updateQandQN :: S.Set T.Text
                 -> (From, To)
                 -> Calendar
                 -> Maybe Calendar
    updateQandQN set (from, to) cal = do
      zipper <- goToNode (from, to) cal
      updatedZip <- update set zipper 
      (root, _) <- upToRoot updatedZip
      return root

