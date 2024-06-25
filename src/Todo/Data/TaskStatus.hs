{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Todo.Data.TaskStatus
  ( TaskStatus (..),
    parseTaskStatus,
    isCompleted,
    render,
  )
where

import Data.Aeson qualified as Asn
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as T
import System.Console.Pretty qualified as Pretty
import Todo.Render.Utils (ColorSwitch (ColorOff, ColorOn))
import Todo.Render.Utils qualified as Render.Utils
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Prelude

-- | Task status.
data TaskStatus
  = Completed
  | InProgress
  | NotStarted
  | Blocked (NESet TaskId)
  deriving stock (Eq, Ord, Show)

instance Semigroup TaskStatus where
  Blocked xs <> Blocked ys = Blocked (xs <> ys)
  l <> r = max l r

instance Monoid TaskStatus where
  mempty = Completed

instance FromJSON TaskStatus where
  parseJSON = Asn.withText "TaskStatus" parseTaskStatus

parseTaskStatus :: forall m. (MonadFail m) => Text -> m TaskStatus
parseTaskStatus txt = do
  foldMappersAltA txt parsers >>= \case
    Nothing -> fail $ "Unexpected status value: " <> quoteTxt txt
    Just r -> pure r
  where
    parsers :: List (Text -> m (Maybe TaskStatus))
    parsers =
      [ parseBlocked,
        parseExact "not-started" NotStarted,
        parseExact "in-progress" InProgress,
        parseExact "completed" Completed
      ]

    parseBlocked :: Text -> m (Maybe TaskStatus)
    parseBlocked (T.stripPrefix "blocked:" -> Just rest) =
      case T.strip rest of
        "" -> fail $ "Received empty list for 'blocked' status: " <> quoteTxt rest
        nonEmpty ->
          let splitStrs = T.strip <$> T.split (== ',') nonEmpty
              nonEmpties = filter (not . T.null) splitStrs
           in case nonEmpties of
                (x : xs) -> do
                  (y :<|| ys) <- traverse TaskId.parseTaskId (x :<|| listToSeq xs)
                  pure $ Just $ Blocked (NESet.fromList (y :| toList ys))
                [] -> fail $ "Received no non-empty ids for 'blocked' status: " <> quoteTxt rest
    parseBlocked _ = pure Nothing

    parseExact :: Text -> TaskStatus -> Text -> m (Maybe TaskStatus)
    parseExact e c t =
      pure
        $ if e == t
          then Just c
          else Nothing

    quoteTxt :: Text -> String
    quoteTxt t = "'" <> unpack t <> "'"

instance ToJSON TaskStatus where
  toJSON (Blocked ts) = toJSON $ "blocked: " <> TaskId.taskIdsToText ts
  toJSON NotStarted = "not-started"
  toJSON InProgress = "in-progress"
  toJSON Completed = "completed"

-- | True iff Completed.
isCompleted :: TaskStatus -> Bool
isCompleted Completed = True
isCompleted _ = False

-- | Renders to Builder.
render :: ColorSwitch -> TaskStatus -> Builder
render ColorOff Completed = "completed"
render ColorOff InProgress = "in-progress"
render ColorOff (Blocked tids) =
  displayBuilder $ "blocked: " <> TaskId.taskIdsToTextQuote tids
render ColorOff NotStarted = "not-started"
render ColorOn Completed = Render.Utils.colorBuilder Pretty.Green "completed"
render ColorOn InProgress = Render.Utils.colorBuilder Pretty.Yellow "in-progress"
render ColorOn (Blocked tids) =
  Render.Utils.colorBuilder Pretty.Red $ "blocked: " <> TaskId.taskIdsToTextQuote tids
render ColorOn NotStarted = Render.Utils.colorBuilder Pretty.Cyan "not-started"
