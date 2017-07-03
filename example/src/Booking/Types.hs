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


-- | Application type
type App a = ReaderT ConfigDB (ExceptT ServantErr IO) a

newtype ConfigDB = Config {
  path :: Text
} deriving Show

runAction :: SqlPersistM a -> App a -- ^ run DB actions in App context
runAction action = reader path >>= liftIO . flip runSqlite action


-- | Booking Types
data CheckInOut = Check {
    checkIn  :: UTCTime
  , checkOut :: UTCTime
} deriving (Show, Generic)

instance FromJSON CheckInOut
instance ToJSON CheckInOut

data Room = Room {
    id   :: Text
  , name :: Text
} deriving (Show, Generic)

instance FromJSON Room
instance ToJSON Room

data Reservation = Reservation {
    name    :: Text
  , check   :: CheckInOut
  , roomIds :: Set Text
} deriving (Show, Generic)

instance FromJSON Reservation
instance ToJSON Reservation

data Report = Report {
    total     :: Set Text
  , reserved  :: Set Text
  , remaining :: Set Text
} deriving (Show, Generic)

instance FromJSON Report
instance ToJSON Report