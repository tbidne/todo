module Todo.Data.TaskId.Internal
  ( TaskId (..),
    mkTaskId,
    parseTaskId,

    -- * Validation
    ValidateResult (..),
    validateText,

    -- * Misc
    mkA,
  )
where

import Data.Aeson qualified as Asn
import Data.Set qualified as Set
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
mkTaskId = mkA "Task id" UnsafeTaskId

mkA :: Text -> (Text -> a) -> Text -> Either String a
mkA name onSuccess txt = case validateText txt of
  InvalidEmpty ->
    Left $ "Received empty " <> unpack lowerName <> ": " <> quoted
  InvalidBadChar c ->
    Left
      $ mconcat
        [ unpack name,
          " '",
          unpack txt,
          "' contained invalid char (',', '<', '>'): ",
          [c]
        ]
  Valid stripped -> Right $ onSuccess stripped
  where
    lowerName = T.toLower name
    quoted = "'" <> unpack txt <> "'"

data ValidateResult
  = Valid Text
  | InvalidEmpty
  | InvalidBadChar Char

validateText :: Text -> ValidateResult
validateText txt
  | T.null stripped = InvalidEmpty
  | Just c <- findBadChar stripped = InvalidBadChar c
  | otherwise = Valid stripped
  where
    stripped = T.strip txt

findBadChar :: Text -> Maybe Char
findBadChar = T.find (`Set.member` badChars)

badChars :: Set Char
badChars = Set.fromList [',', '<', '>']

-- | Parses task id.
parseTaskId :: (MonadFail m) => Text -> m TaskId
parseTaskId txt = case mkTaskId txt of
  Left err -> fail err
  Right x -> pure x
