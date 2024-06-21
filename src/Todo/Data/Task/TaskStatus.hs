{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Todo.Data.Task.TaskStatus
  ( TaskStatus (..),
    isCompleted,
    render,
  )
where

import Data.Aeson qualified as Asn
import Data.Aeson.Types (Parser)
import Data.Set qualified as Set
import Data.Text qualified as T
import System.Console.Pretty qualified as Pretty
import Todo.Data.Task.Render.Utils qualified as Render.Utils
import Todo.Data.Task.TaskId (TaskId (unTaskId))
import Todo.Data.Task.TaskId qualified as TaskId
import Todo.Prelude

-- | Task status.
data TaskStatus
  = Completed
  | InProgress
  | NotStarted
  | Blocked (NESeq TaskId)
  deriving stock (Eq, Ord, Show)

instance Semigroup TaskStatus where
  Blocked xs <> Blocked ys = Blocked (combineUnique xs ys)
  l <> r = max l r

combineUnique :: (Ord a) => NESeq a -> NESeq a -> NESeq a
combineUnique xs ys = unsafeListToNESeq $ Set.toList s
  where
    s = Set.fromList (toList $ xs <> ys)

instance Monoid TaskStatus where
  mempty = Completed

instance FromJSON TaskStatus where
  parseJSON = Asn.withText "TaskStatus" $ \t ->
    foldMappersAltA t parsers >>= \case
      Nothing -> fail $ "Unexpected status value: " <> T.unpack t
      Just r -> pure r
    where
      parsers :: List (Text -> Parser (Maybe TaskStatus))
      parsers =
        [ parseBlocked,
          parseExact "not-started" NotStarted,
          parseExact "in-progress" InProgress,
          parseExact "completed" Completed
        ]

      parseBlocked :: Text -> Parser (Maybe TaskStatus)
      parseBlocked (T.stripPrefix "blocked:" -> Just rest) =
        case T.strip rest of
          "" -> fail $ "Received empty list for 'blocked' status: " <> quoteTxt rest
          nonEmpty ->
            let splitStrs = T.strip <$> T.split (== ',') nonEmpty
                nonEmpties = filter (not . T.null) splitStrs
             in case nonEmpties of
                  (x : xs) -> do
                    ids <- traverse TaskId.parseTaskId (x :<|| listToSeq xs)
                    pure $ Just $ Blocked ids
                  [] -> fail $ "Received no non-empty ids for 'blocked' status: " <> quoteTxt rest
      parseBlocked _ = pure Nothing

      parseExact :: Text -> TaskStatus -> Text -> Parser (Maybe TaskStatus)
      parseExact e c t =
        pure
          $ if e == t
            then Just c
            else Nothing

      quoteTxt :: Text -> String
      quoteTxt t = "'" <> unpack t <> "'"

instance ToJSON TaskStatus where
  toJSON (Blocked ts) = toJSON $ "blocked: " <> TaskId.neSeqToText ts
  toJSON NotStarted = "not-started"
  toJSON InProgress = "in-progress"
  toJSON Completed = "completed"

-- | True iff Completed.
isCompleted :: TaskStatus -> Bool
isCompleted Completed = True
isCompleted _ = False

-- | Renders to Builder.
render :: Bool -> TaskStatus -> Builder
render False Completed = "completed"
render False InProgress = "in-progress"
render False (Blocked tids) = displayBuilder $ "blocked: " <> taskIdsToText tids
render False NotStarted = "not-started"
render True Completed = Render.Utils.colorBuilder Pretty.Green "completed"
render True InProgress = Render.Utils.colorBuilder Pretty.Yellow "in-progress"
render True (Blocked tids) = Render.Utils.colorBuilder Pretty.Red $ "blocked: " <> taskIdsToText tids
render True NotStarted = Render.Utils.colorBuilder Pretty.Cyan "not-started"

taskIdsToText :: NESeq TaskId -> Text
taskIdsToText = T.intercalate ", " . toList . fmap (\t -> showt t.unTaskId)
