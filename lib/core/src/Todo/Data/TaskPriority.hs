module Todo.Data.TaskPriority
  ( TaskPriority (..),
    parseTaskPriority,
    metavar,
  )
where

import Data.Aeson qualified as Asn
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

metavar :: (IsString a) => a
metavar = "(low | normal | high)"
