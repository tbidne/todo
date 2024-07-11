module Todo.Data.Sorted
  ( -- * Types
    SortedTasks,
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
  ( SortedTasks (UnsafeSortedTasks),
  )
import Todo.Data.Task
  ( SingleTask,
    SomeTask (SomeTaskGroup, SomeTaskSingle),
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
    UnsafeSortedTasks $ sortSomeTasks True defSort xs
  Just sortType -> UnsafeSortedTasks $ sortSomeTasks False (toOrd sortType) xs
  where
    toOrd SortPriority =
      cSomeTask (\x -> (Down x.priority, x.taskId))
    toOrd SortStatus =
      cSomeTask (\x -> (Down x.status, x.taskId))
    toOrd SortPriorityStatus =
      cSomeTask (\x -> (Down x.priority, Down x.status, x.taskId))
    toOrd SortStatusPriority =
      cSomeTask (\x -> (Down x.status, Down x.priority, x.taskId))

    defSort = cSomeTask (\x -> (Down x.priority, Down x.status, x.taskId))

sortSomeTasks ::
  -- | Partition completed tasks?
  Bool ->
  -- | Comparison function.
  (SomeTask -> SomeTask -> Ordering) ->
  List SomeTask ->
  List SomeTask
sortSomeTasks partitionCompleted c xs =
  if partitionCompleted
    then sortFn incompleteTasks <> sortFn completedTasks
    else sortFn xs
  where
    sortFn :: List SomeTask -> List SomeTask
    sortFn = fmap (sortSomeTaskSubtasks partitionCompleted c) . L.sortBy c

    (completedTasks, incompleteTasks) = L.partition Task.someTaskIsCompleted xs

sortSomeTaskSubtasks :: Bool -> (SomeTask -> SomeTask -> Ordering) -> SomeTask -> SomeTask
sortSomeTaskSubtasks _ _ t@(SomeTaskSingle _) = t
sortSomeTaskSubtasks partitionCompleted c (SomeTaskGroup t) =
  SomeTaskGroup $ sortTaskGroupSubtasks partitionCompleted c t

sortTaskGroupSubtasks :: Bool -> (SomeTask -> SomeTask -> Ordering) -> TaskGroup -> TaskGroup
sortTaskGroupSubtasks partitionCompleted c t = t {subtasks = subtasks'}
  where
    -- TODO: Avoid the Seq <-> List conversion (Either replace a type or
    -- use Foldable).
    subtasks' =
      Seq.fromList $ sortSomeTasks partitionCompleted c (toList t.subtasks)

cSomeTask :: (Ord a) => (SomeTask -> a) -> SomeTask -> SomeTask -> Ordering
cSomeTask f x y = f x `compare` f y

traverseSorted :: (SingleTask -> a) -> (TaskGroup -> a) -> SortedTasks -> List a
traverseSorted f g = traverseSomeTasks f g . (.unSortedTasks)
