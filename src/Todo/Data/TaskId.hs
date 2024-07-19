module Todo.Data.TaskId
  ( -- * Type
    TaskId,

    -- * Parsing
    Internal.parseTaskId,
    unsafeTaskId,

    -- * Validation
    ValidateResult (..),
    Internal.validateText,

    -- * Rendering
    render,
    taskIdsToText,
    taskIdsToTextQuote,
    taskIdsToTextCustom,

    -- * Misc
    Internal.mkA,
  )
where

import Data.Text qualified as T
import System.Console.Pretty qualified as Pretty
import Todo.Data.TaskId.Internal
  ( TaskId,
    ValidateResult (InvalidBadChar, InvalidEmpty, Valid),
  )
import Todo.Data.TaskId.Internal qualified as Internal
import Todo.Prelude
import Todo.Render.Utils (ColorSwitch (ColorOff, ColorOn))
import Todo.Render.Utils qualified as Render.Utils

-- | Unsafe task id creation.
unsafeTaskId :: (HasCallStack) => Text -> TaskId
unsafeTaskId txt = case Internal.mkTaskId txt of
  Left err -> error err
  Right x -> x

-- | Renders a TaskId.
render :: ColorSwitch -> TaskId -> Builder
render ColorOff = displayBuilder . (.unTaskId)
render ColorOn = Render.Utils.colorBuilder Pretty.Blue . (.unTaskId)

-- | taskIdsToTextCustom with no extra logic.
taskIdsToText :: (Foldable f) => f TaskId -> Text
taskIdsToText = taskIdsToTextCustom (.unTaskId)

-- | taskIdsToTextCustom that single-quotes the text
taskIdsToTextQuote :: (Foldable f) => f TaskId -> Text
taskIdsToTextQuote = taskIdsToTextCustom (\t -> quote t.unTaskId)
  where
    quote t = "'" <> t <> "'"

-- | Comma separates TaskIds together, using the provided function.
taskIdsToTextCustom :: (Foldable f) => (TaskId -> Text) -> f TaskId -> Text
taskIdsToTextCustom toText =
  T.intercalate ", "
    . fmap toText
    . toList
