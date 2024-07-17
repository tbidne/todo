{-# LANGUAGE UndecidableInstances #-}

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

instance
  (k ~ A_Getter, a ~ List SomeTask, b ~ List SomeTask) =>
  LabelOptic "unSortedTasks" k SortedTasks SortedTasks a b
  where
  labelOptic = to (\(UnsafeSortedTasks tl) -> tl)
  {-# INLINE labelOptic #-}
