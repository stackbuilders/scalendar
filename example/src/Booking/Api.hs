{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Booking.Api where

import           Booking.Types
import           Data.Set      (Set)
import           Data.Text     (Text)
import           Servant


type BookingAPI = "hotelbooking" :>
       -- ^ 1) Return a set of availables rooms based on the CheckInOut dates
  (    "getAvailableRooms" :> ReqBody '[JSON] CheckInOut :> Get '[JSON] (Set Room)
       -- ^ 2) Return a boolean if the given RoomId is available
  :<|> "isRoomAvailable" :> ReqBody '[JSON] Text :> Get '[JSON] Bool
       -- ^ 3) Return a report set based on a set of CheckInOut dates
  :<|> "getPeriodicReport" :> ReqBody '[JSON] (Set CheckInOut) :> Get '[JSON] (Set Report)
       -- ^ 4) Creates a reservation based on a Reservation data
  :<|> "booking" :> ReqBody '[JSON] Reservation :> Post '[JSON] Reservation
       -- ^ 5) Removes a reservation based on the ReservationId
  :<|> "cancel" :> ReqBody '[JSON] Text :> Post '[JSON] Reservation
  )
