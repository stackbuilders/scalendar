module Booking.Interpreter where

import           Booking.Api
import           Booking.Types
import           Control.Monad.Except
import           CRUD.Operations
import           Data.Set
import qualified Data.Set                  as S (fromList)
import           Data.Text
import qualified Data.Text                 as T (pack)
import           Servant
import           Time.SCalendar.Operations
import qualified Time.SCalendar.Types      as SC

totalRooms :: Set Text
totalRooms = S.fromList $ (T.pack . show) <$> [100..120]

-- type App a = ReaderT ConfigDB (ExceptT ServantErr IO) a
liftMaybe :: ServantErr -> Maybe a -> App a
liftMaybe err Nothing = lift $ throwError err
liftMaybe _ (Just a) = pure a

postReservation :: ReservationInfo -> App Reservation
postReservation (ReservationInfo name' (Check cIn cOut) roomIds') = do
  reservations <- runAction getAllReservations
  calendar <- liftMaybe undefined $ SC.createSCalendar 2018 12 1 365 totalRooms
  undefined
