{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Schemas.Booking where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateBooking"] [persistLowerCase|
Room
  name     Text
  deriving Show
Reservation
  name     Text
  checkIn  UTCTime
  checkOut UTCTime
  roomIds  Text
  deriving Show
|]
