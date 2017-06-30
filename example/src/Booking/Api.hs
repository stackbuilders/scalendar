{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module Booking.Api where

import           Booking.Types
import           Data.Set      (Set)
import           Data.Text     (Text)
import           Servant

data OneRoom = OneRoom {
    roomId     :: Text
  , checkInOut :: CheckInOut
}

data MultipleRooms = MultipleRooms {
    roomIds    :: Set Text
  , checkInOut :: CheckInOut
}


type BookingAPI = "hotelbooking" :>
  ( "getAvailableRooms"
    :> ReqBody '[JSON] CheckInOut :> Get '[JSON] (Set Room)
  )
  :>
  ( "isRoomAvailable"
    :> ReqBody '[JSON] OneRoom :> Get '[JSON] Bool
  )
  :>
  ( "areRoomsAvailable"
    :> ReqBody '[JSON] MultipleRooms :> Get '[JSON] Bool
  )
  :>
  ( "getPeriodicReport"
    :> ReqBody '[JSON] (Set CheckInOut) :> Get '[JSON] (Set Report)
  )
  :>
  ( "booking"
    :> ReqBody '[JSON] Reservation :> Post '[JSON] Reservation
  )
  :>
  ( "cancel"
    :> ReqBody '[JSON] Text :> Post '[JSON] Reservation
  )
