{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Booking.API where

import           Booking.Types
import           Data.Set      (Set)
import           Data.Text     (Text)
import           Servant

type RoomId = Text

type BookingAPI = "hotelbooking" :>
  (
       -- ^ 1) Return a set of availables rooms based on the CheckInOut dates
       "getAvailableRooms" :> ReqBody '[JSON] CheckInOut :> Get '[JSON] (Set RoomId)
       -- ^ 2) Return a boolean if the given RoomId is available
  :<|> "isRoomAvailable" :> Capture "roomId" RoomId :> ReqBody '[JSON] CheckInOut :> Get '[JSON] Bool
       -- ^ 3) Return a report of the current state of reservations in the given period of time
  :<|> "getPeriodicReport" :> ReqBody '[JSON] CheckInOut :> Get '[JSON] Report
       -- ^ 4) Creates a reservation based on a Reservation data
  :<|> "booking" :> ReqBody '[JSON] ReservationInfo :> Post '[JSON] Reservation
  )
