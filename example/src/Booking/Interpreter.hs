{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Booking.Interpreter where

import           Booking.API
import           Booking.Types
import           Control.Monad             (mapM)
import           Control.Monad.Except
import           Control.Monad.Reader
import           CRUD.Operations
import           Data.Monoid               ((<>))
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
import           Data.ByteString.Lazy           (ByteString)
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


--
-- Calendar constants --

-- | Total rooms in our imaginary hotel represented as ids from 100 to 120.
totalRooms :: Set Text
totalRooms = S.fromList $ (T.pack . show) <$> ([100..120] :: [Int])

-- | For simplicity we are assuming that all the reservations are made in a one
--   year Calendar starting starting from January 1th of 2017.
calendarYear :: Integer
calendarYear = 2017

calendarDay :: Int
calendarDay = 1

calendarMonth :: Int
calendarMonth = 1

calendarSpan :: Int
calendarSpan = 365
-- --

thirtyDays :: NominalDiffTime -- ^ Thirty days in seconds
thirtyDays = 2592000

intervalErrorMsg :: ByteString
intervalErrorMsg =  "Invalid time interval: Check-Out must be greater than or "
                 <> "equal to Check-In and the interval should not span more "
                 <> "than 29 days."

--
-- Handlers --

getAvailableRooms :: CheckInOut -> App (Set RoomId)
getAvailableRooms (Check cIn cOut) = do
  scalendar <- getSCalendarWithReservs (cIn, cOut)
  period <- liftMaybe err500 $ getTimePeriodFromUTC cIn cOut
  (SC.Report _ _ _ remaining') <- liftMaybe (err404 { errBody = "Invalid time check-in and check-out" }) $
    periodReport period scalendar
  pure remaining'

checkReservation :: Text -> CheckInOut -> App Bool
checkReservation roomId (Check cIn cOut) = do
  scalendar <- getSCalendarWithReservs (cIn, cOut)
  reservToCheck <- liftMaybe err500 $ tupleToReserv (cIn, cOut, T.pack . show $ [roomId])
  pure $ isReservAvailable reservToCheck scalendar

getReport :: CheckInOut -> App Report
getReport (Check cIn cOut) = do
  scalendar <- getSCalendarWithReservs (cIn, cOut)
  period <- liftMaybe err500 $ getTimePeriodFromUTC cIn cOut
  (SC.Report _ total' reserved' remaining') <- liftMaybe (err404 { errBody = "Invalid time check-in and check-out" }) $
    periodReport period scalendar
  pure $ Report total' reserved' remaining'

postReservation :: ReservationInfo -> App Reservation
postReservation reservInfo@(ReservationInfo name' (Check cIn cOut) roomIds') = do
  liftMaybe (err400 { errBody = intervalErrorMsg }) $ isValidTimeInterval (cIn, cOut)
  scalendar <- getSCalendarWithReservs (cIn, cOut)
  reservToCheck <- liftMaybe err500 $ tupleToReserv (cIn, cOut, ids)
  if isReservAvailable reservToCheck scalendar
    then
      runAction $ insertReservation name' (cIn, cOut) ids >>= pure . flip Reservation reservInfo
    else
      lift $ throwError (err400 { errBody = "Invalid Reservation" })
  where
    ids = (T.pack . show) $ S.toList roomIds'


--
-- Helpers --

-- | Given a time interval (start, end), this functions creates an SCalendar
--   filled with the reservations included in the interval (start - 30 days, end + 30 days).
--   This is appropriate for hotels since a reservation must be less than 30 days.
--   Note that we do not need to fill the calendar with all reservations from DB, but if
--   you have constraints for your reservations in your business model, say, a reservation
--   cannot be more than N days, then you just have to fill your calendar with
--   the reservations included in (start - N, end - N).
getSCalendarWithReservs :: (UTCTime, UTCTime)
                        -> App SCalendar
getSCalendarWithReservs (cIn, cOut) = do
  let cIn' = (-thirtyDays) `addUTCTime` cIn
      cOut' = thirtyDays `addUTCTime` cOut
  calReservs <- runAction (getReservationsFromPeriod (cIn', cOut')) >>= tuplesToCalReservs
  (SCalendar _ cal) <- liftMaybe err500 $
    createSCalendar calendarYear calendarDay calendarMonth calendarSpan totalRooms
  calWithReservs <- liftMaybe err500 $ reserveManyPeriods' calReservs cal
  pure $ SCalendar totalRooms calWithReservs

-- | Valid hotel reservations are greater than one day but less than 30 days.
isValidTimeInterval :: (UTCTime, UTCTime) -> Maybe ()
isValidTimeInterval (UTCTime gregDayIn _, UTCTime gregDayOut _) =
  let numDays = fromIntegral $ diffDays gregDayOut gregDayIn
  in if numDays < 30 && numDays > 0 then Just () else Nothing

-- | Create a TimePeriod from start and end dates.
getTimePeriodFromUTC :: UTCTime -> UTCTime -> Maybe TimePeriod
getTimePeriodFromUTC (UTCTime gregDayIn _) (UTCTime gregDayOut _) =
  let numDays = fromIntegral $ diffDays gregDayOut gregDayIn
      (year, month, day) = toGregorian gregDayIn
  in makeTimePeriod year month day numDays

tuplesToCalReservs :: [(a, b, UTCTime, UTCTime, Text)] -> App [SC.Reservation]
tuplesToCalReservs tupReservs = liftMaybe err500 $
  mapM (tupleToReserv . (\(_, _, a, b, c) -> (a, b, c))) tupReservs

liftMaybe :: ServantErr -> Maybe a -> App a
liftMaybe err Nothing = lift $ throwError err
liftMaybe _ (Just a) = pure a

tupleToReserv :: (UTCTime, UTCTime, Text) -> Maybe SC.Reservation
tupleToReserv (cIn', cOut', ids') =
  getTimePeriodFromUTC cIn' cOut' >>=
    flip makeReservation (S.fromList $ T.pack <$> (read . T.unpack) ids')


--
-- Application Server --

runContext :: ConfigDB -> App :~> Handler
runContext config = Nat $ flip runReaderT config

bookingProxy :: Proxy BookingAPI
bookingProxy = Proxy

handlers :: ServerT BookingAPI App
handlers = getAvailableRooms
      :<|> checkReservation
      :<|> getReport
      :<|> postReservation

server :: ConfigDB -> Server BookingAPI
server config = enter (runContext config) handlers
