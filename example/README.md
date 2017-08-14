# scalendar Tutorial


# Introduction

Imagine that you have a small hotel with 20 rooms that you have identified from
`room-100` to `room-120`. This example implements a small Servant-API to get
information about its reservation history starting from `January 1th of 2018`.

The API exposes 4 endpoints for:

- creating a reservation for a given time period.

- returning a set of available rooms in a given time period.

- determining if a given room is available in a time period.

- returning a report of the reservation history of the hotel in a time period.


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