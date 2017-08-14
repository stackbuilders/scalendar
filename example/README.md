# scalendar example


# Introduction

Imagine that you have a small hotel with 20 rooms that you have identified from
`room-100` to `room-120`. This example implements a small [Servant-API](https://haskell-servant.readthedocs.io/en/stable/)
to get information about its reservation history starting from `January 1th of 2018`.

The API exposes 4 endpoints for:

- creating a reservation for a given time period.

- returning a set of available rooms in a given time period.

- determining if a given room is available in a time period.

- returning an availability report of the hotel in a time period.


# Running the example

1. Build the project:

```
stack build
```

2. Run the `sqlite` migrations

```
stack exec runMigration
```

3. Start the server
```
stack exec runExample
```

The server will start running on port `3000`.


# Example requests to the server

You can send requests to the server with a client application like [curl](https://curl.haxx.se/). Let's
try some example requests:

1. Reserving rooms `101`, `102` and `103` from February 15th to February 20th of 2017

```
curl -iXPOST localhost:3000/hotelbooking/booking -H "Content-Type: application/json" -d '{
    "name": "Jeremy",
    "check": {
        "checkIn": "2017-02-15T00:00:00Z",
        "checkOut": "2017-02-20T00:00:00Z"
    },
    "roomIds": ["101", "102", "103"]
   }'
```

2. Determining if room `101` is available from February 17th to February 21th of 2017


```
curl -iXGET localhost:3000/hotelbooking/isRoomAvailable/101 -H "Content-Type: application/json" -d '{
      "checkIn": "2017-02-17T00:00:00Z",
      "checkOut": "2017-02-21T00:00:00Z"
    }'
```

If you made the above reservation, you should get a `false` from the server meaning that that room
is not available for that period of time.


3. Returning an availabilty report from February 10th to February 20th of 2017

```
curl -iXGET localhost:3000/hotelbooking/getPeriodicReport -H "Content-Type: application/json" -d '{
      "checkIn": "2017-02-10T00:00:00Z",
      "checkOut": "2017-02-20T00:00:00Z"
    }'
```

If you made the above reservation, the set of reserved rooms for that period of time should include `101`,
`102` and `103`.


4. Returning the rooms which are still available from February 10th to February 20th of 2017

```
curl -iXGET localhost:3000/hotelbooking/getAvailableRooms -H "Content-Type: application/json" -d '{
      "checkIn": "2017-02-10T00:00:00Z",
      "checkOut": "2017-02-20T00:00:00Z"
    }'
```

If you made the above reservation, you should get all the rooms except `101`, `102` and `103`.



# Business Model Constraints and Calendars

In this example implementation we have assumed that hotel reservations must be less than 30 days.
That's why if we need to calculate the availability of a reservation from, say, February 10th to February 20th of 2017,
we do not have to retrieve all the reservations stored in DB and fill the calendar with them, which would be inefficient.
We just need to retrieve the reservations included in an interval of the form `(February 10th - 30 days, February 20th + 30 days)`,
and that's all the Calendar needs to correctly determine the availabilty of a reservation from February 10th to 20th -
this only holds if you properly store reservations in DB taking care that if they are not less than 30 days,
then they cannot be performed.

In general, you should detect if your use case has this kind of time reservation constraint for N days - which
is generally the case since nothing can be reserved forever -, and only allow the storage in DB of reservations less
than N days. Then, when building a Calendar, you should only fetch reservations within a time interval of the form
`(start-date - N, end-date + N)`.