module Time.SCalendar.Internal
  ( getInterval
  , daysBetween
  , goToNode
  , updateQ
  , intervalFitsCalendar
  , checkQuantAvailability
  , checkReservAvailability
  , updateCalendar
  , topMostNodes
  , leftMostTopNode
  , rightMostTopNode
  , commonParent
  , getZipInterval
  , getQMax
  ) where


import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Set (Set, union, unions, size, difference, isSubsetOf)
import Data.Maybe (listToMaybe, maybe)
import Control.Monad (guard)
import Time.SCalendar.Zippers
import Time.SCalendar.Types
import qualified Data.Time as TM (diffUTCTime)


-- | UTILITY FUNCTIONS | --

-- | Given two dates, determine the number of days, inclusively, between them.
daysBetween :: UTCTime -> UTCTime -> Int
daysBetween from to = 1 + round (TM.diffUTCTime to from / oneDay)

-- | A TimePeriod fits a Calendar if it is included in the TimePeriod of the root node of
--   that Calendar and that Calendar is not a Unit.
intervalFitsCalendar :: TimePeriod -> Calendar -> Bool
intervalFitsCalendar interval1 (Node interval2 _  _  _  _) =
  isIncluded (toTimeUnit interval1) (toTimeUnit interval2)
intervalFitsCalendar _ _ = False

-- | Given a node this function returns the interval that that node represents.
getInterval :: Calendar -> TimePeriod
getInterval (Unit unit _ _) = unit
getInterval (Node interval _ _ _ _) = toTimeUnit interval

-- | Like getInterval, but gets the interval from a zipper.
getZipInterval :: CalendarZipper -> TimePeriod
getZipInterval (node, _) = (toTimeUnit . getInterval) node

-- | Get the Q set from a node.
getQ :: CalendarZipper -> Set Text
getQ (Unit _ q _, _) = q
getQ (Node _ q _ _ _, _) = q

-- | Get the QN set from a node.
getQN :: CalendarZipper -> Set Text
getQN (Unit _ _ qn, _) = qn
getQN (Node _ _  qn _ _, _) = qn

-- | Given a node this function returns the QMax For that node.
--   QMax = Q + U(QN of parent nodes up to the root node)
getQMax :: CalendarZipper -> Maybe (Set Text)
getQMax zipper@(_, []) = Just $ getQ zipper
getQMax zipper = do
  parent <- goUp zipper
  go parent (getQ zipper)
  where
    go zipper@(_, []) sum = do
      let qn = getQN zipper
      return $ union sum  qn
    go zipper sum = do
      let qn = getQN zipper
      parent <- goUp zipper
      go parent $ union sum  qn
-- | UTILITY FUNCTIONS | --


-- | Given a period of time and a calendar, this function finds the leftMost top-node
--   of that interval.
leftMostTopNode :: TimePeriod
                -> Calendar
                -> Maybe CalendarZipper
leftMostTopNode interval calendar = do
  guard $ intervalFitsCalendar interval calendar
  result <- ltmNode (getFrom interval, getTo interval) (toZipper calendar)
  listToMaybe result
  where
    ltmNode (lower, upper) zipper@(Unit t _ _, _)
      | lower == getFrom t && getFrom t <= upper = Just [zipper]
      | otherwise = Just []
    ltmNode i@(lower, upper) node@(Node t _ _ _ _, _) = do
      let (from, to) = (getFrom t, getTo t)
      (lChild, bsl) <- goLeft node
      (rChild, bsr) <- goRight node
      if lower == from && to <= upper
        then Just [node]
        else do
          let toL = (getTo . getInterval) lChild
              fromR = (getFrom . getInterval) rChild
          lAnswer <-
            if lower <= toL
              then ltmNode i (lChild, bsl)
              else Just []
          rAnswer <-
            if lower >= fromR
              then ltmNode i (rChild, bsr)
              else Just []
          return $ lAnswer ++ rAnswer

-- | Given a period of time and a calendar, this function finds the rightMost
--   top-node of that interval.
rightMostTopNode :: TimePeriod
                 -> Calendar
                 -> Maybe CalendarZipper
rightMostTopNode interval calendar = do
  guard $ intervalFitsCalendar interval calendar
  result <- rtmNode (getFrom interval, getTo interval) (toZipper calendar)
  listToMaybe result
  where
    rtmNode (lower, upper) zipper@(Unit t _ _, _)
      | upper == getFrom t && getFrom t >= lower = Just [zipper]
      | otherwise = Just []
    rtmNode i@(lower, upper) node@(Node t _ _ _ _, _) = do
      let (from, to) = (getFrom t, getTo t)
      (lChild, bsl) <- goLeft node
      (rChild, bsr) <- goRight node
      if upper == to && from >= lower
        then Just [node]
        else do
          let toL = (getTo . getInterval) lChild
              fromR = (getFrom . getInterval) rChild
          lAnswer <-
            if upper <= toL
              then rtmNode i (lChild, bsl)
              else Just []
          rAnswer <-
            if upper >= fromR
              then rtmNode i (rChild, bsr)
              else Just []
          return $ lAnswer ++ rAnswer

-- | Given two nodes this function finds the common parent node of those nodes,
--   if it exists. The result is only valid if the two nodes (and their zippers) come
--   from the same calendar.
commonParent :: CalendarZipper -> CalendarZipper -> Maybe CalendarZipper
commonParent zipper1@(node1, bs1) zipper2@(node2, bs2) = do
  let bs1Length = length bs1
      bs2Length = length bs2
      interval1 = getInterval node1
      interval2 = getInterval node2
  case bs1Length `compare` bs2Length of
    LT -> do
      zipper2' <- goUp zipper2
      commonParent zipper1 zipper2'
    EQ ->
      if interval1 == interval2
        then Just zipper1
        else do
          zipper1' <- goUp zipper1
          zipper2' <- goUp zipper2
          commonParent zipper1' zipper2'
    GT -> do
      zipper1' <- goUp zipper1
      commonParent zipper1' zipper2

-- | This function returns the topmost nodes of a period of time in a given calendar.
--   This function returns at least the rightmost top-node in case it is found but the leftmost-top
--   node is not found.
topMostNodes :: TimePeriod
             -> Calendar
             -> Maybe [CalendarZipper]
topMostNodes interval calendar = do
  rtNode <- rightMostTopNode (toTimeUnit interval) calendar
  let intervalR = getZipInterval rtNode
  if intervalR == interval
  then return [rtNode]
  else do
    ltNode <- leftMostTopNode interval calendar
    let intervalL = getZipInterval ltNode
    parent <- commonParent ltNode rtNode
    answer <- goDownTree (toTimeUnit interval) intervalL intervalR parent
    return $ ltNode : rtNode : answer
  where
    goDownTree period leftMost rightMost zipper@(Unit t _ _, _) =
      if isIncluded t period
      then if t == rightMost || t == leftMost
           then Just []
           else Just [zipper]
      else Just []
    goDownTree period leftMost rightMost node@(Node t _ _ _ _, _) =
      if isIncluded t period
      then if t == rightMost || t == leftMost
           then Just []
           else Just [node]
      else do
        lChild <- goLeft node
        rChild <- goRight node
        lAnswer <- goDownTree period  leftMost rightMost lChild
        rAnswer <- goDownTree period  leftMost rightMost rChild
        return $ lAnswer ++ rAnswer

-- | This function receives a Node (point in a Calendar) and returns a new Node
--   (up to the root node) with Q updated. That is, this function updates the Q all
--   over the tree. Q = U(Q(leftChild), Q(rightChild), QN).
updateQ :: CalendarZipper -> Maybe CalendarZipper
updateQ zipper@(node, []) = Just zipper
updateQ zipper = do
  parent <- goUp zipper
  lChild <- goLeft parent
  rChild <- goRight parent
  let (Node period q qn left right, bs) = parent
      lQ = getQ lChild
      rQ = getQ rChild
  updateQ (Node period (unions [lQ, rQ, qn]) qn left right, bs)

-- | Given a period of time and a Calendar, go to the node that represents
--   that period, if it exists.
goToNode :: TimePeriod -> Calendar -> Maybe CalendarZipper
goToNode interval calendar = do
  result <- go (toTimeUnit interval) (toZipper calendar)
  listToMaybe result
  where
    go interval zipper@(Unit t _ _, _)
      | interval == t = Just [zipper]
      | otherwise = Just []
    go interval node@(Node t _ _ _ _, bs) =
      if interval == t
      then Just [node]
      else do
        (lChild, bsl) <- goLeft node
        (rChild, bsr) <- goRight node
        let (lower, upper) = (getFrom interval, getTo interval)
            toL = (getTo . getInterval) lChild
            fromR = (getFrom . getInterval) rChild
        lAnswer <- if lower >= fromR
                   then Just []
                   else go interval (lChild, bsl)
        rAnswer <- if upper <=  toL
                   then Just []
                   else go interval (rChild, bsr)
        return $ lAnswer ++ rAnswer

-- | This function takes a list of topMostNodes intervals, a set of units to be reserved,
--   a calendar, and a binary operation over sets (union or difference). The binary
--   operation determines the way that each node will be updated: union for
--   reservations and difference for deleting from a previous reservation.
updateCalendar :: [TimePeriod]
               -> Set  Text
               -> Calendar
               -> (Set Text -> Set Text -> Maybe (Set  Text))
               -> Maybe Calendar
updateCalendar [] _ cal _ = Just cal
updateCalendar (interval:ins) elts cal f = do
  updatedRoot <- updateQandQN elts (toTimeUnit interval) cal
  updateCalendar ins elts updatedRoot f
  where
    update s (Unit unit q qn, bs) = do
      newQ <- f q s
      newQN <- f qn s
      let zipper = (Unit unit newQ newQN, bs)
      updateQ zipper -- ^ make sure to update Q all over the tree.
    update s (Node interval q qn left right, bs) = do
      newQ <- f q s
      newQN <- f qn s
      let zipper = (Node interval newQ newQN left right, bs)
      updateQ zipper -- ^ make sure to update Q all over the tree.
    -- ^ --
    updateQandQN set interval cal = do
      zipper <- goToNode interval cal
      updatedZip <- update set zipper
      (root, _) <- upToRoot updatedZip
      return root

-- | Algorithm to check availability in a calendar for a given Quantity.
checkQuantAvailability :: TimePeriod
                       -> Int
                       -> Set Text
                       -> CalendarZipper
                       -> Bool
checkQuantAvailability interval qt units zipper =
  maybe False (not . null) $ checkAvailability interval qt units zipper
  where
    checkAvailability _ qt units zipper@(Unit {}, _) = do
      qMax <- getQMax zipper
      let avUnits = size (difference units qMax)
      if qt <= avUnits
        then Just [()]
        else Nothing
    checkAvailability interval qt units node@(Node t _ _ _ _, _) = do
      qMax <- getQMax node
      let avUnits = size (difference units qMax)
      -- ^ Propagate a Nothing if conditions are not met
      guard $ qt <= avUnits || not (isIncluded t interval)
      (lChild, bsl) <- goLeft node
      (rChild, bsr) <- goRight node
      let (lower, upper) = (getFrom interval, getTo interval)
          toL = (getTo . getInterval) lChild
          fromR = (getFrom . getInterval) rChild
      lAnswer <-
        if lower >= fromR
          then Just []
          else checkAvailability interval qt units (lChild, bsl)
      rAnswer <-
        if upper <= toL
          then Just []
          else checkAvailability interval qt units (rChild, bsr)
      return $ lAnswer ++ rAnswer

-- | Algorithm to check availability in a calendar for a given Reservation.
checkReservAvailability :: Reservation
                        -> Set Text
                        -> CalendarZipper
                        -> Bool
checkReservAvailability reservation units zipper =
  maybe False (not . null) $ checkAvailability reservation units zipper
  where
    checkAvailability reservation units zipper@(Unit {}, _) = do
      qMax <- getQMax zipper
      let avUnits = difference units qMax
          isSubset = isSubsetOf (reservUnits reservation) avUnits
      if isSubset
        then Just [()]
        else Nothing
    checkAvailability reservation units node@(Node t _ _ _ _, _) = do
      qMax <- getQMax node
      let interval = (toTimeUnit . reservPeriod) reservation
          avUnits = difference units qMax
          isSubset = isSubsetOf (reservUnits reservation) avUnits
      -- ^ If rUnits is not a subset of avUnits and (from, to) is included in (lower, upper),
      --   then there's no availability. Thus propagate a Nothing
      guard $ isSubset || not (isIncluded t interval)
      (lChild, bsl) <- goLeft node
      (rChild, bsr) <- goRight node
      let (lower, upper) = (getFrom interval, getTo interval)
          toL = (getTo . getInterval) lChild
          fromR = (getFrom . getInterval) rChild
      lAnswer <-
        if lower >= fromR
          then Just []
          else checkAvailability reservation units (lChild, bsl)
      rAnswer <-
        if upper <= toL
          then Just []
          else checkAvailability reservation units (rChild, bsr)
      return $ lAnswer ++ rAnswer
