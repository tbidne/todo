module Todo.Data.Task.TaskId
  ( -- * Type
    TaskId (unTaskId),

    -- * Functions
    parseTaskId,
    unsafeTaskId,
    render,
    neSeqToText,
    neSeqToTextCustom,
  )
where

import Data.Text qualified as T
import System.Console.Pretty qualified as Pretty
import Todo.Data.Task.Render.Utils (ColorSwitch (ColorOff, ColorOn))
import Todo.Data.Task.Render.Utils qualified as Render.Utils
import Todo.Data.Task.TaskId.Internal
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

-- | neSeqToTextCustom with no extra logic.
neSeqToText :: NESeq TaskId -> Text
neSeqToText = neSeqToTextCustom (.unTaskId)

-- | Comma separates TaskIds together, using the provided function.
neSeqToTextCustom :: (TaskId -> Text) -> NESeq TaskId -> Text
neSeqToTextCustom toText ids = T.intercalate ", " (toText <$> toList ids)
