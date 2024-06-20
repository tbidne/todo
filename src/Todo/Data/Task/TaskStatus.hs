{-# LANGUAGE ViewPatterns #-}

module Todo.Data.Task.TaskStatus
  ( TaskStatus (..),
    isCompleted,
    render,
  )
where

import Data.Aeson qualified as Asn
import Data.Set qualified as Set
import Data.Text qualified as T
import System.Console.Pretty qualified as Pretty
import Todo.Data.Task.Render.Utils qualified as Render.Utils
import Todo.Data.Task.TaskId (TaskId (MkTaskId, unTaskId))
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
    case foldMappersAlt t parsers of
      Just r -> pure r
      Nothing -> fail $ "Unexpected value: " <> T.unpack t
    where
      parsers =
        [ parseBlocked,
          parseExact "not-started" NotStarted,
          parseExact "in-progress" InProgress,
          parseExact "completed" Completed
        ]
      parseBlocked (T.stripPrefix "blocked: " -> Just rest) =
        let idStrs = T.strip <$> T.split (== ',') rest
         in case idStrs of
              (x : xs) -> Just $ Blocked $ MkTaskId <$> (x :<|| listToSeq xs)
              [] -> Nothing -- TODO: Improve error message
      parseBlocked _ = Nothing

      parseExact e c t =
        if e == t
          then Just c
          else Nothing

instance ToJSON TaskStatus where
  toJSON (Blocked ts) = toJSON $ "blocked: " <> TaskId.neSeqToText ts
  toJSON NotStarted = "not-started"
  toJSON InProgress = "in-progress"
  toJSON Completed = "completed"

isCompleted :: TaskStatus -> Bool
isCompleted Completed = True
isCompleted _ = False

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
