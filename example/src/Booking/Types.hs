{-# LANGUAGE OverloadedStrings #-}

module Booking.Types where

import qualified Data.Set  as S
import qualified Data.Text as T
import qualified Data.Time as TM

type CheckIn = MkCheckIn TM.UTCTime
type CheckOut = MkCheckOut TM.UTCTime
type Name = T.Text

data Room = MkRoom {
  id   :: Integer,
  name :: Name
} deriving (Show)

data Reservation = MkReservation {
  name     :: Name,
  checkIn  :: CheckIn,
  checkOut :: CheckOut,
  rooms    :: [Room]
} deriving (Show)
