{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module Booking.Api where

import           Booking.Types
import           Data.Set      (Set)
import           Data.Text     (Text)
import           Servant


type BookingAPI = "hotelbooking" :>
  -- | Return a set of availables rooms based on the CheckInOut dates
  ( "getAvailableRooms"
    :> ReqBody '[JSON] CheckInOut :> Get '[JSON] (Set Room)
  )
  :>
  -- | Return a bolean if the given RoomId is available
  ( "isRoomAvailable"
    :> ReqBody '[JSON] Text :> Get '[JSON] Bool
  )
  :>
  -- | Return a report set based on a set of CheckInOut dates
  ( "getPeriodicReport"
    :> ReqBody '[JSON] (Set CheckInOut) :> Get '[JSON] (Set Report)
  )
  :>
  -- | Creates a reservation based on a Reservation data
  ( "booking"
    :> ReqBody '[JSON] Reservation :> Post '[JSON] Reservation
  )
  :>
  -- | Removes a reservation based on the ReservationId
  ( "cancel"
    :> ReqBody '[JSON] Text :> Post '[JSON] Reservation
  )
