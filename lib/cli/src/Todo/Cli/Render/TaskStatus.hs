module Todo.Cli.Render.TaskStatus
  ( render,
  )
where

import Data.Text qualified as T
import System.Console.Pretty qualified as Pretty
import Todo.Cli.Prelude
import Todo.Cli.Render.Utils (ColorSwitch (ColorOff, ColorOn))
import Todo.Cli.Render.Utils qualified as Render.Utils
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskStatus
  ( Blocker (BlockerId, BlockerText),
    TaskStatus
      ( Blocked,
        Completed,
        InProgress,
        NotStarted
      ),
  )

-- | Renders to Builder.
render :: ColorSwitch -> TaskStatus -> Builder
render ColorOff Completed = "completed"
render ColorOff InProgress = "in-progress"
render ColorOff (Blocked tids) =
  displayBuilder $ "blocked: " <> renderBlockers tids
render ColorOff NotStarted = "not-started"
render ColorOn Completed = Render.Utils.colorBuilder Pretty.Green "completed"
render ColorOn InProgress = Render.Utils.colorBuilder Pretty.Yellow "in-progress"
render ColorOn (Blocked tids) =
  Render.Utils.colorBuilder Pretty.Red $ "blocked: " <> renderBlockers tids
render ColorOn NotStarted = Render.Utils.colorBuilder Pretty.Cyan "not-started"

-- | taskIdsToTextCustom that single-quotes the text
renderBlockers :: NESet Blocker -> Text
renderBlockers = mapBlockedCustom quote (angleBracket . (.unTaskId))
  where
    quote t = "'" <> t <> "'"

-- | Comma separates TaskIds together, using the provided function.
mapBlockedCustom ::
  (Text -> Text) ->
  (TaskId -> Text) ->
  NESet Blocker ->
  Text
mapBlockedCustom onText onTaskId =
  T.intercalate ", "
    . fmap g
    . toList
  where
    g (BlockerText t) = onText t
    g (BlockerId tid) = onTaskId tid

-- TODO: This logic is a bit fragile, in the sense that we have invariants
-- spread across several functions...consider make this more principled
-- e.g. using optics' Iso to represent to/from Text with angle brackets.
angleBracket :: (IsString a, Semigroup a) => a -> a
angleBracket t = "<" <> t <> ">"
