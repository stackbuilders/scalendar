module Time.SCalendar.DataTypes where


import Data.Set (Set)
import Data.Time (UTCTime)
import Data.Text (Text)


type NumDays = Int
type FirstDay = UTCTime
type CurrentDay = UTCTime
type From = UTCTime
type To = UTCTime
type Day = UTCTime
type Quantity = Int
type TotalUnits = Set Text

-- | Q(Node) = U(Q(LeftChild), Q(RightChild)) U QN(Node)
type Q = Set Text

-- | QN(Node) = reserved elements for all reservations having this node as top-node
type QN = Set Text

-- | QMax = Q + U(QN of parent nodes up to the root node)
type QMax = Set Text

-- | The union of all Qmax's in a list.
type SQMax = Set Text

-- | The difference between TotalUnits and SQMax
type Remaining = Set Text


data Reservation = Reservation (Set Text) (From, To)
  deriving (Eq, Show)

data Cancellation = Cancellation (Set Text) (From, To)
  deriving (Eq, Show)

data Report = Report (From, To) TotalUnits SQMax Remaining
  deriving (Eq, Show)

data Calendar = TimeUnit Day Q QN
              | Empty (From, To)
              | Node (From, To) Q QN Calendar Calendar
              deriving (Eq, Show)

-- | An SCalendar is a Set of identifiers togeher with a Calendar
data SCalendar = SCalendar TotalUnits Calendar
                deriving (Show, Eq)
