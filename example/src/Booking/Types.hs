{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Booking.Types where

import Data.Text (Text)
import Control.Monad.Except
import Control.Monad.Reader
import Servant
import Database.Persist.Sqlite
import Database.Persist.Sql
import Data.Time (UTCTime)
import qualified Data.Set  as S


-- | Application type
type App a = ReaderT ConfigDB (ExceptT ServantErr IO) a

newtype ConfigDB = Config {
    path :: Text
  } deriving Show

runAction :: SqlPersistM a -> App a -- ^ run DB actions in App context
runAction action = reader path >>= liftIO . (flip runSqlite) action


-- | Booking Types
newtype CheckIn = MkCheckIn UTCTime
  deriving (Show)

newtype CheckOut = MkCheckOut UTCTime
  deriving (Show)

type Name = Text

data Room = MkRoom {
    id   :: Integer
  , name :: Name
} deriving (Show)

data Reservation = MkReservation {
    name     :: Name
  , checkIn  :: CheckIn
  , checkOut :: CheckOut
  , rooms    :: [Room] -- ^ sets or lists?
} deriving (Show)
