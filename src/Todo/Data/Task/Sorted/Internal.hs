module Todo.Data.Task.Sorted.Internal
  ( SortedTasks (..),
  )
where

import Todo.Data.Task (SomeTask)
import Todo.Prelude

-- | Sorted tasks.
newtype SortedTasks = UnsafeSortedTasks {unSortedTasks :: List SomeTask}
  deriving stock (Eq, Show)
