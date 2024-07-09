module Todo.Index.Internal
  ( Index (..),
  )
where

import Todo.Data.Task (SomeTask)
import Todo.Prelude

-- | Todo index.
data Index = UnsafeIndex
  { -- | Task list.
    taskList :: List SomeTask,
    -- | Path.
    path :: OsPath
  }
  deriving stock (Eq, Show)
