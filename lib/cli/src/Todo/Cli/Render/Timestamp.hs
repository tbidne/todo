module Todo.Cli.Render.Timestamp
  ( render,
  )
where

import Data.Time.Clock qualified as C
import Data.Time.LocalTime
  ( LocalTime (LocalTime),
    TimeZone,
    ZonedTime (ZonedTime),
  )
import Data.Time.LocalTime qualified as LT
import System.Console.Pretty (Color (Cyan, Magenta, Red))
import Todo.Cli.Prelude
import Todo.Cli.Render.Utils (ColorSwitch (ColorOff, ColorOn))
import Todo.Cli.Render.Utils qualified as Render.Utils
import Todo.Data.Timestamp (Timestamp (Date, Local, Zoned))
import Todo.Data.Timestamp qualified as Timestamp

render :: ColorSwitch -> ZonedTime -> Timestamp -> Builder
render ColorOff _ ts = displayBuilder $ Timestamp.formatTimestamp ts
render ColorOn zt ts = Render.Utils.colorBuilder c (pack $ Timestamp.formatTimestamp ts)
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
