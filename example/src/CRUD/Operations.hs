module CRUD.Operations where

import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           Database.Persist.Sql
import           Schemas.Booking

insertReservation :: Text -> (UTCTime, UTCTime) -> Text -> SqlPersistM Integer
insertReservation name (cin, cout) rooms =
  insert (Reservation name cin cout rooms) >>= pure . fromIntegerTokey

updateReservationRooms :: Integer -> Text -> SqlPersistM ()
updateReservationRooms rId rooms =
  update (ReservationKey . SqlBackendKey . fromInteger $ rId)
         [ReservationRoomIds =. rooms]

getReservationsFromPeriod :: (UTCTime, UTCTime)
                          -> SqlPersistM [(Integer, Text, UTCTime, UTCTime, Text)]
getReservationsFromPeriod (cIn, cOut) = do
  entities <- selectList [ ReservationCheckIn >=. cIn
                         , ReservationCheckIn <=. cOut
                         , ReservationCheckOut >=. cIn
                         , ReservationCheckOut <=. cOut ] []
  pure $ (\(Entity key (Reservation name cin cout rooms))
    -> (fromIntegerTokey key, name, cin, cout, rooms)) <$> entities

fromIntegerTokey :: Key Schemas.Booking.Reservation -> Integer
fromIntegerTokey = toInteger . unSqlBackendKey . unReservationKey
