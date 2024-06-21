module Todo.Data.Task.TaskId.Internal
  ( TaskId (..),
    parseTaskId,
  )
where

import Data.Aeson qualified as Asn
import Data.Text qualified as T
import Todo.Prelude

-- | Task id.
newtype TaskId = UnsafeTaskId {unTaskId :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord, ToJSON)

instance FromJSON TaskId where
  parseJSON = Asn.withText "TaskId" parseTaskId

-- | Parses task id.
parseTaskId :: (MonadFail m) => Text -> m TaskId
parseTaskId txt
  | T.null stripped = fail $ "Received empty task id: " <> quoted
  | T.elem ',' stripped = fail $ "Task id contained comma: " <> quoted
  | otherwise = pure $ UnsafeTaskId stripped
  where
    stripped = T.strip txt
    quoted = "'" <> unpack txt <> "'"
