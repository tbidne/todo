module Todo.Data.TaskId
  ( -- * Type
    TaskId (unTaskId),

    -- * Functions
    parseTaskId,
    unsafeTaskId,
    render,
    taskIdsToText,
    taskIdsToTextQuote,
    taskIdsToTextCustom,
  )
where

import Data.Text qualified as T
import System.Console.Pretty qualified as Pretty
import Todo.Render.Utils (ColorSwitch (ColorOff, ColorOn))
import Todo.Render.Utils qualified as Render.Utils
import Todo.Data.TaskId.Internal
  ( TaskId (unTaskId),
    mkTaskId,
    parseTaskId,
  )
import Todo.Prelude

-- | Unsafe task id creation.
unsafeTaskId :: (HasCallStack) => Text -> TaskId
unsafeTaskId txt = case mkTaskId txt of
  Left err -> error err
  Right x -> x

-- | Renders a TaskId.
render :: ColorSwitch -> TaskId -> Builder
render ColorOff = displayBuilder . (.unTaskId)
render ColorOn = Render.Utils.colorBuilder Pretty.Blue . (.unTaskId)

-- | taskIdsToTextCustom with no extra logic.
taskIdsToText :: NESet TaskId -> Text
taskIdsToText = taskIdsToTextCustom (.unTaskId)

-- | taskIdsToTextCustom that single-quotes the text
taskIdsToTextQuote :: NESet TaskId -> Text
taskIdsToTextQuote = taskIdsToTextCustom (\t -> quote t.unTaskId)
  where
    quote t = "'" <> t <> "'"

-- | Comma separates TaskIds together, using the provided function.
taskIdsToTextCustom :: (TaskId -> Text) -> NESet TaskId -> Text
taskIdsToTextCustom toText =
  T.intercalate ", "
    . fmap toText
    . toList
