{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Booking.Interpreter where

import           Booking.Api
import           Booking.Types
import           Control.Monad             (mapM)
import           Control.Monad.Except
import           Control.Monad.Reader
import           CRUD.Operations
import           Data.Proxy
import           Data.Set
import qualified Data.Set                  as S (fromList, toList)
import           Data.Text
import qualified Data.Text                 as T (pack, unpack)
import           Data.Time                 (UTCTime (..), diffUTCTime,
                                            toGregorian)
import           Servant
import           Time.SCalendar.Operations (isReservAvailable,
                                            reserveManyPeriods')
import           Time.SCalendar.Types      (SCalendar (..), createSCalendar,
                                            makeReservation, makeTimePeriod)

totalRooms :: Set Text
totalRooms = S.fromList $ (T.pack . show) <$> [100..120]

-- type App a = ReaderT ConfigDB (ExceptT ServantErr IO) a
liftMaybe :: ServantErr -> Maybe a -> App a
liftMaybe err Nothing = lift $ throwError err
liftMaybe _ (Just a) = pure a

postReservation :: ReservationInfo -> App Reservation
postReservation reservInfo@(ReservationInfo name' (Check cIn cOut) roomIds') = do
  reservations <- runAction getAllReservations
  calReservs <- liftMaybe err500 $ mapM tupleToReserv reservations
  (SCalendar _ cal) <- liftMaybe err500 $ createSCalendar 2018 12 1 365 totalRooms
  calWithReservs <- liftMaybe err500 $ reserveManyPeriods' calReservs cal
  reservToCheck <- liftMaybe err500 $ tupleToReserv (0, "", cIn, cOut, ids)
  if isReservAvailable reservToCheck (SCalendar totalRooms calWithReservs)
    then
      runAction $ insertReservation name' (cIn, cOut) ids >>= pure . flip Reservation reservInfo
    else
      lift $ throwError err400
  where
    tupleToReserv (_, _, cIn', cOut', ids) = do
      let (UTCTime gregDay _) = cIn'
          oneDay = 86400
          numDays = round $ diffUTCTime cOut cIn / oneDay
          (year, month, day) = toGregorian gregDay
      timePeriod <- makeTimePeriod year month day numDays
      makeReservation timePeriod (S.fromList $ T.pack <$> (read . T.unpack) ids)
    ids = (T.pack . show) $ S.toList roomIds'

runContext :: ConfigDB -> App :~> Handler
runContext config = Nat $ flip runReaderT config

-- | Application Server and Handlers

bookingProxy :: Proxy BookingAPI
bookingProxy = Proxy

handlers :: ServerT BookingAPI App
handlers = undefined :<|> undefined :<|> undefined :<|> postReservation :<|> undefined

server :: ConfigDB -> Server BookingAPI
server config = enter (runContext config) handlers
