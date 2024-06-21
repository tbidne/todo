module Todo.Data.Timestamp
  ( Timestamp (..),
    parseTimestamp,
    formatTimestamp,
    render,
  )
where

import Data.Aeson.Types qualified as Asn
import Data.Time.Calendar (Day)
import Data.Time.Clock qualified as C
import Data.Time.Format qualified as Format
import Data.Time.LocalTime
  ( LocalTime (LocalTime),
    TimeZone,
    ZonedTime (ZonedTime),
  )
import Data.Time.LocalTime qualified as LT
import Effects.Time qualified as Time
import System.Console.Pretty (Color (Cyan, Magenta, Red))
import Todo.Data.Task.Render.Utils qualified as Render.Utils
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

render :: Bool -> ZonedTime -> Timestamp -> Builder
render False _ ts = displayBuilder $ formatTimestamp ts
render True zt ts = Render.Utils.colorBuilder c (pack $ formatTimestamp ts)
  where
    c = timestampToColor zt ts

timestampToColor :: ZonedTime -> Timestamp -> Color
timestampToColor currTime@(ZonedTime _ tz) ts
  | diff <= 0 = Red
  | diff <= fiveDays = Magenta
  | otherwise = Cyan
  where
    fiveDays = C.nominalDay * 5
    diff = C.diffUTCTime tsUTC currTimeUTC
    currTimeUTC = LT.zonedTimeToUTC currTime
    tsZoned = toZoned tz ts
    tsUTC = LT.zonedTimeToUTC tsZoned

toZoned :: TimeZone -> Timestamp -> ZonedTime
toZoned tz (Date day) = ZonedTime (LocalTime day LT.midnight) tz
toZoned tz (Local lt) = ZonedTime lt tz
toZoned _ (Zoned zt) = zt
