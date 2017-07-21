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
                                           , NominalDiffTime
                                           , addUTCTime
                                           , toGregorian
                                           , diffDays   )
import           Servant
import           Time.SCalendar.Operations ( isReservAvailable
                                           , reserveManyPeriods'
                                           , periodReport )
import qualified Time.SCalendar.Types as SC (Reservation, Report(..))
import           Time.SCalendar.Types      ( SCalendar (..)
                                           , TimePeriod
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

thirthyDays :: NominalDiffTime
thirthyDays = 2592000 -- Seconds


-- | Handlers

checkReservation :: Text -> CheckInOut -> App Bool
checkReservation roomId (Check cIn cOut) = do
  scalendar <- getSCalendarWithReservs (cIn, cOut)
  reservToCheck <- liftMaybe err500 $ tupleToReserv (cIn, cOut, T.pack . show $ [roomId])
  pure $ isReservAvailable reservToCheck scalendar


getReport :: CheckInOut -> App Report
getReport (Check cIn cOut) = do
  scalendar <- getSCalendarWithReservs (cIn, cOut)
  period <- liftMaybe err500 $ getTimePeriodFromUTC cIn cOut
  (SC.Report _ total reserved remainig) <- liftMaybe (err404 { errBody = "Invalid time check-in and check-out" }) $
    periodReport period scalendar
  pure $ Report total reserved remainig

postReservation :: ReservationInfo -> App Reservation
postReservation reservInfo@(ReservationInfo name' (Check cIn cOut) roomIds') = do
  scalendar <- getSCalendarWithReservs (cIn, cOut)
  reservToCheck <- liftMaybe err500 $ tupleToReserv (cIn, cOut, ids)
  if isReservAvailable reservToCheck scalendar
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
tupleToReserv (cIn', cOut', ids') =
  getTimePeriodFromUTC cIn' cOut' >>= flip makeReservation (S.fromList $ T.pack <$> (read . T.unpack) ids')

getTimePeriodFromUTC :: UTCTime -> UTCTime -> Maybe TimePeriod
getTimePeriodFromUTC (UTCTime gregDayIn _) (UTCTime gregDayOut _) =
  let numDays = fromIntegral $ diffDays gregDayOut gregDayIn
      (year, month, day) = toGregorian gregDayIn
  in makeTimePeriod year month day numDays

tuplesToCalReservs :: [(a, b, UTCTime, UTCTime, Text)] -> App [SC.Reservation]
tuplesToCalReservs tupReservs = liftMaybe err500 $
  mapM (tupleToReserv . (\(_, _, a, b, c) -> (a, b, c))) tupReservs

getSCalendarWithReservs :: (UTCTime, UTCTime) -> App SCalendar
getSCalendarWithReservs (cIn, cOut) = do
  let cIn' = (-thirthyDays) `addUTCTime` cIn
      cOut' = thirthyDays `addUTCTime` cOut
  calReservs <- runAction (getReservationsFromPeriod (cIn', cOut')) >>= tuplesToCalReservs
  (SCalendar _ cal) <- liftMaybe err500 $
    createSCalendar calendarYear calendarDay calendarMonth calendarSpan totalRooms
  calWithReservs <- liftMaybe err500 $ reserveManyPeriods' calReservs cal
  pure $ SCalendar totalRooms calWithReservs


-- | Application Server

runContext :: ConfigDB -> App :~> Handler
runContext config = Nat $ flip runReaderT config

bookingProxy :: Proxy BookingAPI
bookingProxy = Proxy

handlers :: ServerT BookingAPI App
handlers = undefined :<|> checkReservation :<|> getReport :<|> postReservation :<|> undefined

server :: ConfigDB -> Server BookingAPI
server config = enter (runContext config) handlers
