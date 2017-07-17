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
import           Data.Set                  (Set)
import qualified Data.Set                  as S (fromList, toList)
import           Data.Text                 (Text)
import qualified Data.Text                 as T (pack, unpack)
import           Data.Time                 ( UTCTime (..)
                                           , toGregorian
                                           , diffDays   )
import           Servant
import           Time.SCalendar.Operations ( isReservAvailable
                                           , reserveManyPeriods' )
import qualified Time.SCalendar.Types as SC (Reservation)
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

checkReservation :: Text -> CheckInOut -> App Bool
checkReservation roomId (Check cIn cOut) = do
  calReservs <- runAction getAllReservations >>= tuplesToCalReservs
  (SCalendar _ cal) <- emptyCalendar
  calWithReservs <- liftMaybe err500 $ reserveManyPeriods' calReservs cal
  reservToCheck <- liftMaybe err500 $ tupleToReserv (cIn, cOut, T.pack . show $ [roomId])
  pure $ isReservAvailable reservToCheck (SCalendar totalRooms calWithReservs)

postReservation :: ReservationInfo -> App Reservation
postReservation reservInfo@(ReservationInfo name' (Check cIn cOut) roomIds') = do
  calReservs <- runAction getAllReservations >>= tuplesToCalReservs
  (SCalendar _ cal) <- emptyCalendar
  calWithReservs <- liftMaybe err500 $ reserveManyPeriods' calReservs cal
  reservToCheck <- liftMaybe err500 $ tupleToReserv (cIn, cOut, ids)
  if isReservAvailable reservToCheck (SCalendar totalRooms calWithReservs)
    then
      runAction $ insertReservation name' (cIn, cOut) ids >>= pure . flip Reservation reservInfo
    else
      lift $ throwError (err400 { errBody = "Invalid Reservation" })
  where
    ids = (T.pack . show) $ S.toList roomIds'


-- | Helpers

liftMaybe :: ServantErr -> Maybe a -> App a
liftMaybe err Nothing = lift $ throwError err
liftMaybe _ (Just a) = pure a

tupleToReserv :: (UTCTime, UTCTime, Text) -> Maybe SC.Reservation
tupleToReserv (cIn', cOut', ids') = do
  let (UTCTime gregDayIn _) = cIn'
      (UTCTime gregDayOut _) = cOut'
      numDays = fromIntegral $ diffDays gregDayOut gregDayIn
      (year, month, day) = toGregorian gregDayIn
  timePeriod <- makeTimePeriod year month day numDays
  makeReservation timePeriod (S.fromList $ T.pack <$> (read . T.unpack) ids')

emptyCalendar = liftMaybe err500
  $ createSCalendar calendarYear calendarDay calendarMonth calendarSpan totalRooms

tuplesToCalReservs :: [(a, b, UTCTime, UTCTime, Text)] -> App [SC.Reservation]
tuplesToCalReservs tupReservs = liftMaybe err500 $
  mapM (tupleToReserv . (\(_, _, a, b, c) -> (a, b, c))) tupReservs


-- | Application Server

runContext :: ConfigDB -> App :~> Handler
runContext config = Nat $ flip runReaderT config

bookingProxy :: Proxy BookingAPI
bookingProxy = Proxy

handlers :: ServerT BookingAPI App
handlers = undefined :<|> checkReservation :<|> undefined :<|> postReservation :<|> undefined

server :: ConfigDB -> Server BookingAPI
server config = enter (runContext config) handlers
