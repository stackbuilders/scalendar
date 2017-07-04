module Main where

import           Booking.Config  (exampleConf)
import           Schemas.Booking (runBookingMigration)

main :: IO ()
main = runBookingMigration exampleConf
