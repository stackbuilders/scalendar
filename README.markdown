# SCalendar: Haskell Library to deal with resource availability in a Calendar.
[![CircleCI](https://circleci.com/gh/nebtrx/SCalendar/tree/master.svg?style=svg&circle-token=884fdc7c78c3f4e9241e6c8f962462d48aad81e1)](https://circleci.com/gh/nebtrx/SCalendar/tree/master)

This is a library for handling calendars and resource availability based on the
"top-nodes" algorithm and set operations. That's why it is called `SCalendar`: Calendars
which use Sets.
Since the bare "top-nodes" algorithm is not enough to IDENTIFY which are the
specific resources which are available in a given period of time - for example,
the id of the rooms which can still be reserved in a given checkIn-checkOut
interval - it was necessary to generalize that algorithm to work on sets of
strings which identify the available resources in a period. That generalization
was pretty smooth since addition in numbers was replaced by set Union and substraction
in numbers was replaced by set difference. Another important fact about sets is that
they do not allow duplicates, so every identifier is guaranteed to be unique.


# Introduction


# Data Types

`SCalendar` is a library based on binary trees, where a Calendar is defined as follows:

```
data Calendar = TimeUnit Unit Q QN
              | Empty (From, To)
              | Node (From, To) Q QN Calendar Calendar
```


The idea is that each node in the calendar will store a time interval (From, To)
where both From and To are of type [UTCTime](https://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time-Clock.html):

The purpose is that terminal nodes (leaves) will represent a time unit (for example, a
day). Thus we have Nodes and TimeUnits which are the only type constructors which
store Q and QN sets.
For more information about the time representation according to the `top-nodes`
algorithm check [this](https://en.wikipedia.org/wiki/Top-nodes_algorithm)

The `Empty` Constructor is just to represent an empty Calendar.

Knowing what the `Q` and `QN` sets mean is not quite important to use this library but
roughly:
 - `QN(Node)` represents reserved elements for all reservations having this node as top-node
 - `Q(Node) = U(Q(LeftChild), Q(RightChild)) U QN(Node)`


In order to use this library, it only suffices to know the meaning of the following
data type:

```
data SCalendar = SCalendar TotalUnits Calendar
```


TotalUnits is a type synonym for `(Set T.Text)` and thus a `SCalendar` is only a pair of
a set of identifiers for a group of available resources - for example, the numbers
which are used to identify rooms in a hotel `{"101", "102", ...}` - and a `Calendar`, which
is the tree that handles time intervals.

Other important data types are:


- `data Reservation = Reservation (S.Set T.Text) (From, To)`

  which represents a set of resources we want to reserve in a period of time `(From, To)`
  from a `SCalendar`.


- `data Cancellation = Cancellation (S.Set T.Text) (From, To)`

  which represents a set of resources we want to cancel in a period of time `(From, To)``
  from a `SCalendar`.

- `data Report = Report (From, To) TotalUnits SQMax Remaining`

  which represents a Report for a given period of time where a Report has the following
  information:
   - `TotalUnits`: The set of total identifiers for resources which can be reserved in a
     calendar.
   - `SQMax`: The set of identifiers for resources which have been reserved for a period
     `(From, To)`.
   - `Remaining`: The set of remaining identifiers for resources which can still be
     reserved without creating conflicts in a period of time `(From, To)`.



# Creating a Calendar

All the fundamental operations to manage calendar are located in `SCalendar.Operations`

To create a bare `Calendar` which is not associated to any set of identifiers we can use

```
createCalendar :: UTCTime -> NumDays -> Maybe Calendar
```

where
 - `UTCTime`: The starting point of time of the calendar, for example, it could be
   a calendar starting from `2016-05-04T00:00:00-05:00`.
 - `Numdays`: A type synonym for `Int` which represents the size of the calendar. For
   implementation reasons the size will always be a power of 2. Thus if you
   provide 30 as `NumDays`, the function will choose the first power of 2 which is
   equal or greater than 30, that is, 32.

So if everything is ok, this function Just returns a new `Calendar` which is suitable for
the given `NumDays`. A new `Calendar` is one which has neither reservations nor cancellations.


`createSCalendar` is almost like `createCalendar` but instead of returning a bare calendar,
it returns a `SCalendar` which is a calendar together with a set of strings (of type `Text`)
which uniquely identify a group of available resources. Here is that function's type
and an example:

```
createSCalendar :: UTCTime -> NumDays -> S.Set T.Text -> Maybe SCalendar

start = UTCTime (fromGregorian 2016 07 10) 0

createCalendar start 128 (fromList ["a", "b", "c", "d"])
```

Here we are creating a calendar of 128 days (about 4 months) that starts from
`2016-01-01T00:00:00-05:00`.



# Checking Availability

There are two functions to check availability for a reservation. The first one is

```
isQuantityAvailable :: Int -> (From, To) -> SCalendar -> Bool
```

where Int is an amount of resource we want to reserve, `(From, To)` is the period of
time we want to reserve for that amount of resource, and `SCalendar` is the calendar
where we want to check availability. Naturally, this function returns a bool if the
amount of resources is available.
Note that here we are just concerned with the amount of resources and whether there is
some set of identifiers whose size is greater of equal to that amount. If we need
to check if a particular set of identifiers is available for reservation, we can
use the following function:

```
isReservAvailable :: Reservation -> SCalendar -> Bool
```

which is almost like the first function, but here we are taking into account the set
of strings which identifies the resources we want to reserve since we are providing
a `Reservation` as input. For example:

```
isReservAvailable (Reservation (fromList ["a", "b", "c", "d"]) (from, to))
                  (SCalendar ["a", "b", "c", "d"] calendar)
```

# Adding reservations to a Calendar

There are two pairs of functions to add reservations to a calendar:

```
reservPeriod_ :: Reservation -> Calendar -> Maybe Calendar
```

which inserts reservations into a calendar without any constraint.  That's it, this
function does not apply any availability check before making the reservation. That's
why this function does not need a `SCalendar`, because it does not need to take
into account the set of total available resources.

The safe version is `reservPeriod` (without the underscore) which enforces the
`isReservAvailable` check over that reservation before adding it. Its type is

```
reservePeriod :: Reservation -> SCalendar -> Maybe SCalendar
```

where an `SCalendar` is needed because we are taking into account the set of total
available resources to make the validation.

The other pair of functions are quite similar but are handy for adding a list of
reservations at once:

```
reserveManyPeriods_ :: [Reservation] -> Calendar -> Maybe Calendar
```
which adds several reservations at once in a Calendar without any check.

```
reserveManyPeriods :: [Reservation] -> SCalendar -> Maybe SCalendar
```
which  will return a `SCalendar` only with the reservations that pass the
`isReservAvailable` test. Here we must take into consideration that reservations will be
inserted in the same order they come in the input list. So, if a reservation conflicts
with the ones that have been already inserted, it will not be included in the tree.



# Removing Reservation: Cancellations

There are two operations which allow us to remove reserved resources from a period of
time:

```
cancelPeriod :: Cancellation -> Calendar -> Maybe Calendar
```

This operation takes a `Cancellation` and returns a `Calendar` with that `Cancellation`'s
set of resource identifiers subtracted from that `Calendar` in that `Cancellation`'s
period of time.
Be careful with this operation because there is no restriction over the period you are
deleting. You may be deleting from several reservations, from periods of time which are
meaningless - which have already elapsed-, and so on. However, all this library is
intended to be used together with some persistent storage system which will allow you
to keep record of the exact dates and resources which are reserved or cancelled.


The other operation is

```
cancelManyPeriods :: [Cancellation] -> Calendar -> Maybe Calendar
```
which is handy for cancelling a list of periods at once.

Note that for cancellations we do not need a `SCalendar` because we don't need to make
any availability check.


# One important thing to note

Since this calendar implementation uses `Sets` and `Set` operations, you don't have to worry
about things like updating the total number of resource identifiers for your `SCalendar`.
You can freely remove or add identifiers to your `SCalendar` and there will be no
conflicts while making availability checks, reservations, cancellations, and so on.


# Reports

It is very useful to have an operation which can summarize some information about the
state of the calendar in a given period of time. That's why this library has

```
periodReport :: (From, To) -> SCalendar -> Maybe Report
```

where `(From, To)` is the interval of time you would like the `Report` to summarize and
`SCalendar` is the calendar we are working on. This function returns a report over that
period of time with the following information:
  - `TotalUnits`: The set of total identifiers for resources in the `SCalendar`,
    in other words, the set part of (`SCalendar` set calendar).
  - `SQMax`: The set of resources which have been reserved for that period of time.
  - `Remaining`: The set of remaining  resources which can still be reserved without
    creating conflicts in a period of time `(From, To)`.

Note that `TotalUnits`, `SQMax`, `Remaining` are all of type `Set`, and that `Report` is
something like `(Report Set Set Set)`.


# Have Fun!

So if you find this library useful, have fun with it in applications which need some
sort of calendar and resource availability management!!!!!!!



# Acknowledgements

The base code for this library was written by Sebastián Pulido Gómez.
The author of the algorithm is Rayrole, Martin: "Method and device or arrangement for the management of a resource schedule." U.S. Patent Application No. 10/764,526.
There is an improvement of the algorithm suggested by the author:
     http://www.researchgate.net/publication/311582722_Method_of_Managing_Resources_in_a_Telecommunication_Network_or_a_Computing_System
