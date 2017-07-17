{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Booking.Api where

import           Booking.Types
import           Data.Set      (Set)
import           Data.Text     (Text)
import           Servant

type ReservationId = Text
type RoomId = Text

type BookingAPI = "hotelbooking" :>
  (
       -- ^ 1) Return a set of availables rooms based on the CheckInOut dates
       "getAvailableRooms" :> ReqBody '[JSON] CheckInOut :> Get '[JSON] (Set Room)
       -- ^ 2) Return a boolean if the given RoomId is available
  :<|> "isRoomAvailable" :> Capture "roomId" RoomId :> ReqBody '[JSON] CheckInOut :> Get '[JSON] Bool
       -- ^ 3) Return a report set based on a set of CheckInOut dates
  :<|> "getPeriodicReport" :> ReqBody '[JSON] CheckInOut :> Get '[JSON] (Set Report)
       -- ^ 4) Creates a reservation based on a Reservation data
  :<|> "booking" :> ReqBody '[JSON] ReservationInfo :> Post '[JSON] Reservation
       -- ^ 5) Removes a reservation based on the ReservationId
  :<|> "cancelRooms" :> Capture "reservationId" ReservationId :> ReqBody '[JSON] (Set RoomId) :> Post '[JSON] Reservation
  )
