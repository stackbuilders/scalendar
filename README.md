[![Hackage version](https://img.shields.io/hackage/v/scalendar.svg)](https://hackage.haskell.org/package/scalendar-1.2.0)
[![No Maintenance Intended](http://unmaintained.tech/badge.svg)](http://unmaintained.tech/)

> **⚠️ Warning:** This library has been deprecated and is no longer maintained. It will not receive any further security patches, features, or bug fixes and is preserved here at GitHub for archival purposes. If you want to use it, we suggest forking the repository and auditing the codebase before use. For more information, contact us at info@stackbuilders.com.

# DEPRECATED - scalendar: Haskell Library to deal with resource availability in a Calendar.

This is a library for handling calendars and resource availability based on the
`top-nodes algorithm` and set operations. That's why it is called `scalendar`: Calendars
which which keep track of the availability of a set of resources.
Since the bare `top-nodes algorithm` is not enough to IDENTIFY which are the
specific resources which are available in a given period of time - for example,
the id of the rooms which can still be reserved in a given `(checkIn-checkOut)`
interval - it was necessary to generalize that algorithm to work on sets of
strings which identify the available resources in a period. That generalization
was pretty smooth since addition in numbers was replaced by set Union and substraction
in numbers was replaced by set Difference. Another important fact about sets is that
they do not allow duplicates, so every identifier is guaranteed to be unique.


# Introduction


## Data Types

`scalendar` is a library based on binary trees, where a Calendar is defined as follows:

```
data Calendar =
    Unit TimePeriod (Set Text) (Set Text)
  | Node TimePeriod (Set Text) (Set Text) Calendar Calendar

data TimePeriod =
    TimeInterval UTCTime UTCTime
  | TimeUnit UTCTime
```

The idea is that each node in the calendar will store a TimePeriod which is a data type which stores
a time-interval with an`start-date` and an `end-date`.

The purpose is that terminal nodes (leaves) will represent a unit of time, `TimeUnit`, which in this case
is a nominal day or 86400 seconds. Thus non-terminal nodes are intended to store a `TimeInterval` and
leaves are intended to store `TimeUnits`. Both leaves and nodes store `Q` and `QN` sets, which are the
data structures which allow the Calendar to keep track of the availability of a set of resources.
For more information about the time representation according to the `top-nodes`
algorithm check [this](https://en.wikipedia.org/wiki/Top-nodes_algorithm)

Knowing what the `Q` and `QN` sets mean is not quite important to use this library but
roughly:
 - `QN(Node)` represents reserved elements for all reservations having this node as `top-node`
 - `Q(Node) = U(Q(LeftChild), Q(RightChild)) U QN(Node)`

In order to use this library, it only suffices to know the meaning of the following
data type:

```
data SCalendar = SCalendar
  { calUnits :: Set Text
  , calendar :: Calendar
  } deriving (Eq, Show)
```

An `SCalendar` is only a product type of a set of identifiers and a group of available resources - for
example, the numbers which are used to identify rooms in a hotel `{"101", "102", ...}` - and a `Calendar`,
which is the tree that keeps track of the availability of that set of resources.

Other important data types are:


- ```
  data Reservation = Reservation
    { reservUnits :: Set Text
    , reservPeriod :: TimePeriod
    }
  ```

  which represents a set of resources we want to reserve over a `TimePeriod` in a `SCalendar`.

- ```
  data Cancellation = Cancellation
    { cancUnits :: Set Text
    , cancPeriod :: TimePeriod
    }
  ```

  which represents a set of resources we want to cancel over a `TimePeriod` in a `SCalendar`

- ```
  data Report = Report
    { reportPeriod :: TimePeriod
    , totalUnits :: Set Text
    , reservedUnits :: Set Text
    , remainingUnits :: Set Text
    }
  ```
  which represents a `Report` for a given `TimePeriod` where a `Report` has the following
  information:
   - `totalUnits`: The set of total identifiers for resources which can be reserved in a
     `SCalendar`.
   - `reservedUnits`: The set of identifiers for resources which have been reserved for a `TimePeriod`.
   - `remainingUnits`: The set of remaining identifiers for resources which can still be
      reserved without creating conflicts in a `TimePeriod`.


## Creating a Calendar

Functions to create Calendars are located in `Time.SCalendar.Types`

To create a bare `Calendar` which is not associated to any set of identifiers we can use

```
createCalendar :: Integer -- Year.
               -> Int -- Month.
               -> Int -- Day.
               -> Int -- NumDays.
               -> Maybe Calendar
```

where
 - `Year`: It is an `Integer` representing the starting year of the `Calendar`. For example
    `2017`.
 - `Month`: It is an `Int` representing the starting month of the `Calendar`. For
    example, `2` is equivalent to February.
 - `Day`: It is an `Int` representing the starting day of the `Calendar`. It can be
    any number representing one of the days of `Month`.
 - `NumDays`: It is the number of Days we want our `Calendar` to cover. The days covered
    by it will always be a power of `2`. Thus if you input `30`, `createCalendar` will
    find the first power of `2` which is greater or equal to `32`, in this case `2^5 = 32`.

So if everything is ok, this function `Just` returns a new `Calendar` which is suitable for
the given `NumDays`. A new `Calendar` is one which has neither reservations nor cancellations.

`createSCalendar` is almost like `createCalendar` but instead of returning a bare `Calendar`,
it returns an `SCalendar` which is a `Calendar` together with a set of identifiers (of type `Text`)
which uniquely identify a group of available resources. The following example create an `SCalendar`
of `2 ^ 8 = 512 days` starting from `2016-February-2` with a set of identifiers `{ a, b, c, d }`

```
createSCalendar :: Integer -- Year.
                -> Int -- Month.
                -> Int -- Day.
                -> Int -- NumDays.
                -> Set Text -- Set of Identifiers
                -> Maybe SCalendar

createSCalendar 2017 2 1 365 (Set.fromList ["a", "b", "c", "d"])
```

## Checking Availability

There are two functions to check availability for a reservation. The first one is

```
isQuantityAvailable :: Int -> TimePeriod -> SCalendar -> Bool
```

where `Int` is an amount of resource we want to reserve, `TimePeriod` is the period of
time we want to reserve for that amount of resource, and `SCalendar` is the calendar
where we want to check availability. Naturally, this function returns a `Bool` if the
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
a `Reservation` as input.


## Adding reservations to a Calendar

There are two pairs of functions to add reservations to a calendar:

```
reservPeriod' :: Reservation -> Calendar -> Maybe Calendar
```

which inserts reservations into a calendar without any constraint.  That's it, this
function does not apply any availability check before making the `Reservation`. That's
why this function does not need a `SCalendar`, because it does not need to take
into account the set of total available resources.

The safe version is `reservPeriod` (without the quote) which enforces the
`isReservAvailable` check over that reservation before adding it. Its type is

```
reservePeriod :: Reservation -> SCalendar -> Maybe SCalendar
```

where an `SCalendar` is needed because we are taking into account the set of total
available resources to make the validation.

The other pair of functions are quite similar but are handy for adding a list of
reservations at once:

```
reserveManyPeriods' :: [Reservation] -> Calendar -> Maybe Calendar
```
which adds several reservations at once in a Calendar without any availability check.

```
reserveManyPeriods :: [Reservation] -> SCalendar -> Maybe SCalendar
```
which  will return a `SCalendar` only with the reservations that pass the
`isReservAvailable` test. Here we must take into consideration that reservations will be
inserted in the same order they come in the input list. So, if a reservation conflicts
with the ones that have been already inserted, it will not be included in the `SCalendar`.



## Removing Reservation: Cancellations

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


## One important thing to note

Since this calendar implementation uses `Sets` and `Set` operations, you don't have to worry
about things like updating the total number of resource identifiers for your `SCalendar`.
You can freely remove or add identifiers to your `SCalendar` and there will be no
conflicts while making availability checks, reservations, cancellations, and so on.


## Reports

It is very useful to have an operation which can summarize some information about the
state of the calendar in a given period of time. That's why this library has

```
periodReport :: TimePeriod  -> SCalendar -> Maybe Report
```

where `TimePeriod` is the interval of time you would like the `Report` to summarize and
`SCalendar` is the calendar we are working on. This function returns a `Report` over that
period of time with the following information:
  - `totalUnits`: The set of total identifiers for resources in the `SCalendar`,
    in other words, the set part of `(SCalendar set calendar)`.
  - `reservedUnits`: The set of resources which have been reserved for that period of time.
  - `remainingUnits`: The set of remaining  resources which can still be reserved without
    creating conflicts in a `TimePeriod`.

Note that `totalUnits`, `reservedUnits`, `remainingUnits` are all of type `Set`, and that the type
of `Report` is :

  ```
  data Report = Report
    { reportPeriod :: TimePeriod
    , totalUnits :: Set Text
    , reservedUnits :: Set Text
    , remainingUnits :: Set Text
    }
  ```

## Have Fun!

So if you find this library useful, have fun with it in applications which need some
sort of calendar and resource availability management!!


# Acknowledgements

The base code for this library was written by [Sebastián Pulido Gómez](https://github.com/sebashack) and
was sponsored by [Stack Builders](https://www.stackbuilders.com/)

Thanks to [Mark Karpov](https://github.com/mrkkrp) and [Javier Casas](https://github.com/javcasas) for
their code reviews and suggestions.


## Top-Nodes Algorithm Patent information

The ideas used to implement this library come from an invention by [Martin Rayrole](https://worldwide.espacenet.com/publicationDetails/biblio?locale=en_EP&II=8&FT=D&CC=US&DB=EPODOC&NR=2004204978A1&date=20041014&ND=3&KC=A1&adjacent=true#).

This version of the algorithm invented by Martin Rayrole now does not have any patent protection. You can verify that by clicking on the `Abandonment` section of this [web-page](https://register.epo.org/ipfwretrieve?lng=en&apn=US.76452604.A). Thus this now belongs to the public domain!

---
## License

MIT, see [the LICENSE file](LICENSE).

## Contributing

Do you want to contribute to this project? Please take a look at our [contributing guideline](CONTRIBUTING.md) to know how you can help us build it.

---
<img src="https://cdn.stackbuilders.com/media/images/Sb-supports.original.png" alt="Stack Builders" width="50%"></img>  
[Check out our libraries](https://github.com/stackbuilders/) | [Join our team](https://www.stackbuilders.com/join-us/)
