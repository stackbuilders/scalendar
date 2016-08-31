module SCalendar.DataTypes where

import Control.Monad
import qualified Data.Set as S
import qualified Data.Time as TM
import qualified Data.Text as T


type NumDays = Int
type Quantity = Int
type FirstDay = TM.UTCTime
type CurrentDay = TM.UTCTime
type TotalUnits = S.Set T.Text

type From = TM.UTCTime
type To = TM.UTCTime
type Unit = TM.UTCTime
type Q = S.Set T.Text -- Q(Node) = U(Q(LeftChild), Q(RightChild)) U QN(Node)
type QN = S.Set T.Text -- QN(Node) = reserved elements for all reservations having this node as top-node
type QMax = S.Set T.Text -- QMax = Q + U(QN of parent nodes up to the root node)
type SQMax = S.Set T.Text -- The union of all qmax's in a list.
type Remaining = S.Set T.Text -- the difference between TotalUnits and SQMax


data Reservation = Reservation (S.Set T.Text) (From, To) -- A Reservation must specify which elements will be reserved.
  deriving (Eq, Show)



data Cancellation = Cancellation (S.Set T.Text) (From, To)
  deriving (Eq, Show)



data Report = Report (From, To) TotalUnits SQMax Remaining
  deriving (Eq, Show)



data Calendar = TimeUnit Unit Q QN
              | Empty (From, To)
              | Node (From, To) Q QN Calendar Calendar
              deriving (Eq, Show)


-- This data type represents the fact that a calendar must be attached to a number of available units,
-- that is, a set of identifiers together with a Calendar.
data SCalendar = SCalendar TotalUnits Calendar
                deriving (Show, Eq)
