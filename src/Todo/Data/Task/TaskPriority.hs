module Todo.Data.Task.TaskPriority
  ( TaskPriority (..),
    render,
  )
where

import Data.Aeson qualified as Asn
import System.Console.Pretty qualified as Pretty
import Todo.Data.Task.Render.Utils qualified as Render.Utils
import Todo.Prelude

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
  parseJSON = Asn.withText "TaskPriority" $ \case
    "low" -> pure Low
    "normal" -> pure Normal
    "high" -> pure High
    other -> fail $ "Unexpected value: " <> unpack other

instance ToJSON TaskPriority where
  toJSON Low = "low"
  toJSON Normal = "normal"
  toJSON High = "high"

render :: Bool -> TaskPriority -> Builder
render False Low = "low"
render False Normal = "normal"
render False High = "high"
render True Low = Render.Utils.colorBuilder Pretty.Yellow "low"
render True Normal = Render.Utils.colorBuilder Pretty.Cyan "normal"
render True High = Render.Utils.colorBuilder Pretty.Red "high"
