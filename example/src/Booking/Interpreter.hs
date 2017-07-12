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
import           Data.Time                 ( UTCTime (..)
                                           , toGregorian
                                           , diffDays   )
import           Servant
import           Time.SCalendar.Operations ( isReservAvailable
                                           , reserveManyPeriods' )
import           Time.SCalendar.Types      ( SCalendar (..)
                                           , createSCalendar
                                           , makeReservation
                                           , makeTimePeriod )


-- | Calendar constants
totalRooms :: Set Text
totalRooms = S.fromList $ (T.pack . show) <$> [100..120]

calendarYear :: Integer
calendarYear = 2018

calendarDay :: Int
calendarDay = 1

calendarMonth :: Int
calendarMonth = 1

calendarSpan :: Int
calendarSpan = 365


-- | Handlers

postReservation :: ReservationInfo -> App Reservation
postReservation reservInfo@(ReservationInfo name' (Check cIn cOut) roomIds') = do
  reservations <- runAction getAllReservations
  calReservs <- liftMaybe err500 $
    mapM (tupleToReserv . (\(_, _, a, b, c) -> (a, b, c))) reservations
  (SCalendar _ cal) <- liftMaybe err500
    $ createSCalendar calendarYear calendarDay calendarMonth calendarSpan totalRooms
  calWithReservs <- liftMaybe err500 $ reserveManyPeriods' calReservs cal
  reservToCheck <- liftMaybe err500 $ tupleToReserv (cIn, cOut, ids)
  if isReservAvailable reservToCheck (SCalendar totalRooms calWithReservs)
    then
      runAction $ insertReservation name' (cIn, cOut) ids >>= pure . flip Reservation reservInfo
    else
      lift $ throwError (err400 { errBody = "Invalid Reservation" })
  where
    tupleToReserv (cIn', cOut', ids') = do
      let (UTCTime gregDayIn _) = cIn'
          (UTCTime gregDayOut _) = cOut'
          numDays = fromIntegral $ diffDays gregDayOut gregDayIn
          (year, month, day) = toGregorian gregDayIn
      timePeriod <- makeTimePeriod year month day numDays
      makeReservation timePeriod (S.fromList $ T.pack <$> (read . T.unpack) ids')
    -- | --
    ids = (T.pack . show) $ S.toList roomIds'


-- | Helpers

liftMaybe :: ServantErr -> Maybe a -> App a
liftMaybe err Nothing = lift $ throwError err
liftMaybe _ (Just a) = pure a


-- | Application Server

runContext :: ConfigDB -> App :~> Handler
runContext config = Nat $ flip runReaderT config

bookingProxy :: Proxy BookingAPI
bookingProxy = Proxy

handlers :: ServerT BookingAPI App
handlers = undefined :<|> undefined :<|> undefined :<|> postReservation :<|> undefined

server :: ConfigDB -> Server BookingAPI
server config = enter (runContext config) handlers
