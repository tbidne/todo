module Todo.Index.Internal
  ( Index (..),
  )
where

import Todo.Data.Task (SomeTask)
import Todo.Prelude

-- | Todo index.
newtype Index = UnsafeIndex {unIndex :: List SomeTask}
  deriving stock (Eq, Show)
