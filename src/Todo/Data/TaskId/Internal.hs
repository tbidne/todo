module Todo.Data.TaskId.Internal
  ( TaskId (..),
    mkTaskId,
    parseTaskId,
  )
where

import Data.Aeson qualified as Asn
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Todo.Prelude

-- | Task id.
newtype TaskId = UnsafeTaskId {unTaskId :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord, ToJSON)

instance IsString TaskId where
  fromString str = case mkTaskId (pack str) of
    Left err -> error err
    Right x -> x

instance FromJSON TaskId where
  parseJSON = Asn.withText "TaskId" parseTaskId

-- | Constructs a task id.
mkTaskId :: Text -> Either String TaskId
mkTaskId txt
  | T.null stripped = Left $ "Received empty task id: " <> quoted
  | T.elem ',' stripped = Left $ "Task id contained comma: " <> quoted
  | otherwise = Right $ UnsafeTaskId stripped
  where
    stripped = T.strip txt
    quoted = "'" <> unpack txt <> "'"

-- | Parses task id.
parseTaskId :: (MonadFail m) => Text -> m TaskId
parseTaskId txt = case mkTaskId txt of
  Left err -> fail err
  Right x -> pure x
