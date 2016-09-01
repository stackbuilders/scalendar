# SCalendar: Haskell Library to deal with resource availability in a Calendar.

This is a library for handling calendars and resource availability based on the 
"top-nodes" algorithm and set operations. 
Since the bare "top-nodes" algorithm is not enough to IDENTIFY which are the
specific resources which are available in a given period of time - for example,
the number of the rooms which can still be reserved in a given checkIn-checkOut
interval - it was necessary to generalize that algorithm to work on sets of 
strings which identify the available resources in a period. That generalization
was pretty smooth since addition in numbers was replaced by set Union and substraction
in numbers was replaced by set difference. Another important fact about sets is that
they do not allow duplicated, so every identifier is guaranteed to be unique.


# Introduction


# Data Types

SCalendar is a library based on binary trees, where a Calendar is defined as follows:

data Calendar = TimeUnit Unit Q QN
              | Empty (From, To)
              | Node (From, To) Q QN Calendar Calendar
              

The idea is that each node in the calendar will store a time interval (From, To) 
where both From and To are of type UTCTime: 

https://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time-Clock.html

The idea is that terminal nodes (leaves) will represent a time unit (for example, a
day). Thus we have Nodes and TimeUnits which are the only type constructors which 
store Q and QN sets.
For more information about the time representation according to the "top-nodes"
algorithm see:

https://en.wikipedia.org/wiki/Top-nodes_algorithm

The Empty Constructor is just to representing an empty Calendar.

Knowing what the Q and QN sets mean is not quite important to use this library but
roughly: 
 - QN(Node) = reserved elements for all reservations having this node as top-node
 - Q(Node) = U(Q(LeftChild), Q(RightChild)) U QN(Node)
 

In order to use this library, it only suffices to know the meaning of the following
data type:

data SCalendar = SCalendar TotalUnits Calendar
                 
                 
TotalUnits is a type synonym form Set T.Text and thus an SCalendar is only a pair of
a set of identifiers for a group of available resources - for example, the numbers
which are used to identify rooms in a hotel {"101", "102", ...} - and a Calendar, which
is the tree that handles time intervals.

Other important data types are:


data Reservation = Reservation (S.Set T.Text) (From, To) 
                  
which represents a set of resources we want to reserve in a period of time (from, to)
from a SCalendar.


data Cancellation = Cancellation (S.Set T.Text) (From, To)

which represents a set of resources we want to cancel in a period of time (from, to) 
from a SCalendar.

data Report = Report (From, To) TotalUnits SQMax Remaining

which represents a Report for a given period of time where a Report has the following
information:
  - TotalUnits: The set of total identifiers for resources which can be reserved in a
    calendar.
  - SQMax: The set of identifiers for resources which have been reserved for a period
    (From, To).
  - Remaining: The set of remaining identifiers for resources which can still be 
    reserved without creating conflicts in a period of time (From, To).
    
     
                  







# Author

The base code for this library was originally written by Sebastian Pulido.
