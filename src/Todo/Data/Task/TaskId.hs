module Todo.Data.Task.TaskId
  ( -- * Type
    TaskId (unTaskId),

    -- * Functions
    parseTaskId,
    render,
    neSeqToText,
    neSeqToTextCustom,
  )
where

import Data.Text qualified as T
import System.Console.Pretty qualified as Pretty
import Todo.Data.Task.Render.Utils qualified as Render.Utils
import Todo.Data.Task.TaskId.Internal (TaskId (unTaskId), parseTaskId)
import Todo.Prelude

-- | Renders a TaskId.
render :: Bool -> TaskId -> Builder
render False = displayBuilder . (.unTaskId)
render True = Render.Utils.colorBuilder Pretty.Blue . (.unTaskId)

-- | neSeqToTextCustom with no extra logic.
neSeqToText :: NESeq TaskId -> Text
neSeqToText = neSeqToTextCustom (.unTaskId)

-- | Comma separates TaskIds together, using the provided function.
neSeqToTextCustom :: (TaskId -> Text) -> NESeq TaskId -> Text
neSeqToTextCustom toText ids = T.intercalate ", " (toText <$> toList ids)
