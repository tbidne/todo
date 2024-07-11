module Todo.Index.Internal
  ( Index (..),
  )
where

import Todo.Data.Task (SomeTask)
import Todo.Prelude

-- | Todo index.
data Index = UnsafeIndex (List SomeTask) OsPath
  deriving stock (Eq, Show)

instance HasField "taskList" Index (List SomeTask) where
  getField (UnsafeIndex tl _) = tl

instance HasField "path" Index OsPath where
  getField (UnsafeIndex _ p) = p
