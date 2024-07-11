module Todo.Data.Sorted.Internal
  ( SortedTasks (..),
  )
where

import Todo.Data.Task (SomeTask)
import Todo.Prelude

-- | Sorted tasks.
newtype SortedTasks = UnsafeSortedTasks (List SomeTask)
  deriving stock (Eq, Show)

instance HasField "unSortedTasks" SortedTasks (List SomeTask) where
  getField (UnsafeSortedTasks tl) = tl
