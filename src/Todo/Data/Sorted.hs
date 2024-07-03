module Todo.Data.Sorted
  ( -- * Types
    SortedTasks (unSortedTasks),
    SortType (..),

    -- * Functions
    sortTasks,
    traverseSorted,
  )
where

import Data.List qualified as L
import Data.Ord (Down (Down))
import Data.Sequence qualified as Seq
import Todo.Data.Sorted.Internal
  ( SortedTasks (UnsafeSortedTasks, unSortedTasks),
  )
import Todo.Data.Task
  ( SomeTask (SomeTaskGroup, SomeTaskSingle),
    SingleTask,
    TaskGroup (subtasks),
    traverseSomeTasks,
  )
import Todo.Data.Task qualified as Task
import Todo.Prelude

-- | Sort type.
data SortType
  = -- | Sort by priority.
    SortPriority
  | -- | Sort by status.
    SortStatus
  | -- | Sort by priority then status.
    SortPriorityStatus
  | -- | Sort by status then priority.
    SortStatusPriority
  deriving stock (Eq, Show)

-- | Sorts tasks. Tasks are always sorted by id, so any given sort will
-- take priority, using id as a tie-breaker.
sortTasks ::
  -- | Primary sort type. If 'Nothing', sorts by status then priority, with
  -- all completed tasks appearing at the bottom.
  Maybe SortType ->
  -- | Tasks to sort.
  List SomeTask ->
  SortedTasks
sortTasks mSortType xs = case mSortType of
  Nothing ->
    UnsafeSortedTasks $ defSort incompleteTasks <> defSort completedTasks
  Just sortType -> UnsafeSortedTasks $ sortSomeTasks (toOrd sortType) xs
  where
    toOrd SortPriority =
      cSomeTask (\x -> (Down x.priority, x.taskId))
    toOrd SortStatus =
      cSomeTask (\x -> (Down x.status, x.taskId))
    toOrd SortPriorityStatus =
      cSomeTask (\x -> (Down x.priority, Down x.status, x.taskId))
    toOrd SortStatusPriority =
      cSomeTask (\x -> (Down x.status, Down x.priority, x.taskId))

    (completedTasks, incompleteTasks) = L.partition Task.someTaskIsCompleted xs

    defSort =
      sortSomeTasks (cSomeTask (\x -> (Down x.status, Down x.priority, x.taskId)))

sortSomeTasks ::
  (SomeTask -> SomeTask -> Ordering) ->
  List SomeTask ->
  List SomeTask
sortSomeTasks c =
  fmap (sortSomeTaskSubtasks c)
    . L.sortBy c

sortSomeTaskSubtasks :: (SomeTask -> SomeTask -> Ordering) -> SomeTask -> SomeTask
sortSomeTaskSubtasks _ t@(SomeTaskSingle _) = t
sortSomeTaskSubtasks c (SomeTaskGroup t) =
  SomeTaskGroup $ sortTaskGroupSubtasks c t

sortTaskGroupSubtasks :: (SomeTask -> SomeTask -> Ordering) -> TaskGroup -> TaskGroup
sortTaskGroupSubtasks c t = t {subtasks = Seq.sortBy c t.subtasks}

cSomeTask :: (Ord a) => (SomeTask -> a) -> SomeTask -> SomeTask -> Ordering
cSomeTask f x y = f x `compare` f y

traverseSorted :: (SingleTask -> a) -> (TaskGroup -> a) -> SortedTasks -> List a
traverseSorted f g = traverseSomeTasks f g . (.unSortedTasks)
