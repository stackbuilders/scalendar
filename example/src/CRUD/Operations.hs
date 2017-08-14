module CRUD.Operations where

import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           Database.Persist.Sql
import           Schemas.Booking


insertReservation :: Text
                  -> (UTCTime, UTCTime)  -- ^ check-in and check-out.
                  -> Text                -- ^ Set of rooms in text representation, e.g., "[101, 102, 103]".
                  -> SqlPersistM Integer -- ^ Id of the created reservation in DB.
insertReservation name (cin, cout) rooms =
  insert (Reservation name cin cout rooms) >>= pure . fromIntegerTokey

-- | Get reservations in DB included in a given interval of time.
getReservationsFromPeriod :: (UTCTime, UTCTime)
                          -> SqlPersistM [(Integer, Text, UTCTime, UTCTime, Text)]
getReservationsFromPeriod (cIn, cOut) = do
  entities <- selectList [ ReservationCheckIn >=. cIn
                         , ReservationCheckIn <=. cOut
                         , ReservationCheckOut >=. cIn
                         , ReservationCheckOut <=. cOut ] []
  pure $ (\(Entity key (Reservation name cin cout rooms))
    -> (fromIntegerTokey key, name, cin, cout, rooms)) <$> entities

fromIntegerTokey :: Key Reservation -> Integer
fromIntegerTokey = toInteger . unSqlBackendKey . unReservationKey
