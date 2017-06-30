{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Booking.Types where

import qualified Data.Set  as S
import qualified Data.Text as T
import qualified Data.Time as TM

newtype CheckIn = MkCheckIn TM.UTCTime
  deriving (Show)

newtype CheckOut = MkCheckOut TM.UTCTime
  deriving (Show)

type Name = T.Text

data Room = MkRoom {
    id   :: Integer
  , name :: Name
} deriving (Show)

data Reservation = MkReservation {
    name     :: Name
  , checkIn  :: CheckIn
  , checkOut :: CheckOut
  , rooms    :: [Room] -- sets or lists?
} deriving (Show)
