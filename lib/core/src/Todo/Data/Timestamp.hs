module Todo.Data.Timestamp
  ( Timestamp (..),
    parseTimestamp,
    formatTimestamp,
    metavar,
  )
where

import Data.Aeson.Types qualified as Asn
import Data.Time.Calendar (Day)
import Data.Time.Format qualified as Format
import Data.Time.LocalTime (LocalTime, ZonedTime)
import Data.Time.LocalTime qualified as LT
import Effects.Time qualified as Time
import Todo.Prelude

-- | Task timestamps.
data Timestamp
  = Date Day
  | Local LocalTime
  | Zoned ZonedTime
  deriving stock (Show)

instance Eq Timestamp where
  Date d1 == Date d2 = d1 == d2
  Local lt1 == Local lt2 = lt1 == lt2
  Zoned zt1 == Zoned zt2 = LT.zonedTimeToUTC zt1 == LT.zonedTimeToUTC zt2
  _ == _ = False

instance FromJSON Timestamp where
  parseJSON = Asn.withText "Timestamp" (parseTimestamp . unpack)

instance ToJSON Timestamp where
  toJSON = toJSON . formatTimestamp

parseTimestamp :: (MonadFail m) => String -> m Timestamp
parseTimestamp s = do
  let mParsed =
        asum
          [ Date <$> Format.parseTimeM True Format.defaultTimeLocale dateFormat s,
            Local <$> Time.parseLocalTime s,
            Zoned <$> Time.parseZonedTime s
          ]
  case mParsed of
    Nothing -> fail $ "Could not parse timestamp: " <> s
    Just parsed -> pure parsed

formatTimestamp :: Timestamp -> String
formatTimestamp (Date d) = Format.formatTime Format.defaultTimeLocale dateFormat d
formatTimestamp (Local lt) = Time.formatLocalTime lt
formatTimestamp (Zoned zt) = Time.formatZonedTime zt

dateFormat :: String
dateFormat = "%0Y-%m-%d"

metavar :: (IsString a) => a
metavar = "(YYYY-MM-DD [HH:MM:SS] [TZ])"
