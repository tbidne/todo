module Todo.Data.Task.TaskId
  ( TaskId (..),
    render,
    neSeqToText,
    neSeqToTextCustom,
  )
where

import Data.Text qualified as T
import System.Console.Pretty qualified as Pretty
import Todo.Data.Task.Render.Utils qualified as Render.Utils
import Todo.Prelude

-- TODO: Enforce invariants: non-empty and does not contain comma.

newtype TaskId = MkTaskId {unTaskId :: Text}
  deriving stock (Show)
  deriving newtype (Eq, FromJSON, Ord, ToJSON)

render :: Bool -> TaskId -> Builder
render False = displayBuilder . (.unTaskId)
render True = Render.Utils.colorBuilder Pretty.Blue . (.unTaskId)

neSeqToText :: NESeq TaskId -> Text
neSeqToText = neSeqToTextCustom (.unTaskId)

neSeqToTextCustom :: (TaskId -> Text) -> NESeq TaskId -> Text
neSeqToTextCustom toText ids = T.intercalate ", " (toText <$> toList ids)
