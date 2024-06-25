module Todo.Data.TaskPriority
  ( TaskPriority (..),
    parseTaskPriority,
    render,
  )
where

import Data.Aeson qualified as Asn
import System.Console.Pretty qualified as Pretty
import Todo.Prelude
import Todo.Render.Utils (ColorSwitch (ColorOff, ColorOn))
import Todo.Render.Utils qualified as Render.Utils

-- | Task priority
data TaskPriority
  = Low
  | Normal
  | High
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance Semigroup TaskPriority where
  (<>) = max

instance Monoid TaskPriority where
  mempty = Low

instance FromJSON TaskPriority where
  parseJSON = Asn.withText "TaskPriority" parseTaskPriority

parseTaskPriority :: (MonadFail f) => Text -> f TaskPriority
parseTaskPriority "low" = pure Low
parseTaskPriority "normal" = pure Normal
parseTaskPriority "high" = pure High
parseTaskPriority other = fail $ "Unexpected priority value: " <> unpack other

instance ToJSON TaskPriority where
  toJSON Low = "low"
  toJSON Normal = "normal"
  toJSON High = "high"

render :: ColorSwitch -> TaskPriority -> Builder
render ColorOff Low = "low"
render ColorOff Normal = "normal"
render ColorOff High = "high"
render ColorOn Low = Render.Utils.colorBuilder Pretty.Yellow "low"
render ColorOn Normal = Render.Utils.colorBuilder Pretty.Cyan "normal"
render ColorOn High = Render.Utils.colorBuilder Pretty.Red "high"
