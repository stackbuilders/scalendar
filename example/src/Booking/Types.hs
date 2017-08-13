{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Booking.Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Set                (Set)
import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           GHC.Generics
import           Servant


--
-- Application type --

type App = ReaderT ConfigDB (ExceptT ServantErr IO)

newtype ConfigDB = Config {
  path :: Text
  } deriving Show

runAction :: SqlPersistM a -> App a -- ^ run DB actions in App context
runAction action = reader path >>= liftIO . flip runSqlite action


--
-- Booking Types --

data CheckInOut = Check {
    checkIn  :: UTCTime
  , checkOut :: UTCTime
  } deriving (Show, Generic, Ord, Eq)

data Reservation = Reservation {
    id              :: Integer
  , reservationInfo :: ReservationInfo
  } deriving (Show, Generic)

data ReservationInfo = ReservationInfo {
    name    :: Text
  , check   :: CheckInOut
  , roomIds :: Set Text
  } deriving (Show, Generic)

data Report = Report {
    total     :: Set Text -- ^ Total rooms in our imaginary hotel.
  , reserved  :: Set Text -- ^ Rooms which have been already reserved.
  , remaining :: Set Text -- ^ Rooms which are still available.
  } deriving (Show, Generic)


--
-- FromJSON and ToJSON instances --

instance FromJSON CheckInOut
instance ToJSON CheckInOut

instance FromJSON Reservation
instance ToJSON Reservation

instance FromJSON ReservationInfo
instance ToJSON ReservationInfo

instance FromJSON Report
instance ToJSON Report
