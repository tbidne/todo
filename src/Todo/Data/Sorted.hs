module Todo.Data.Sorted
  ( -- * Types
    SortedTasks,
    SortType (..),

    -- * Functions
    sortTasks,
    traverseSorted,
    parseSortType,
  )
where

import Data.List qualified as L
import Data.Ord (Down (Down))
import TOML (DecodeTOML (tomlDecoder))
import Todo.Configuration.Data.RevSort (RevSort)
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

instance DecodeTOML SortType where
  tomlDecoder = parseSortType tomlDecoder

-- | Sorts tasks. Tasks are always sorted by id, so any given sort will
-- take priority, using id as a tie-breaker.
sortTasks ::
  -- | Primary sort type. If 'Nothing', sorts by status then priority, with
  -- all completed tasks appearing at the bottom.
  Maybe SortType ->
  -- | Reverses the sort.
  RevSort ->
  -- | Tasks to sort.
  List SomeTask ->
  SortedTasks
sortTasks mSortType revSort xs =
  case mSortType of
    Nothing ->
      UnsafeSortedTasks $ sortSomeTasks True revSort defSort' xs
    Just sortType -> UnsafeSortedTasks $ sortSomeTasks False revSort (toOrd' sortType) xs
  where
    (toOrd', defSort') =
      if revSort ^. #boolIso
        then (toOrdRev, defSortRev)
        else (toOrd, defSort)

    toOrd SortPriority =
      cSomeTask (\x -> (Down x.priority, x.taskId))
    toOrd SortStatus =
      cSomeTask (\x -> (Down x.status, x.taskId))
    toOrd SortPriorityStatus =
      cSomeTask (\x -> (Down x.priority, Down x.status, x.taskId))
    toOrd SortStatusPriority =
      cSomeTask (\x -> (Down x.status, Down x.priority, x.taskId))

    toOrdRev sortType t1 t2 =
      case toOrd sortType t1 t2 of
        EQ -> EQ
        LT -> GT
        GT -> LT

    defSort = cSomeTask (\x -> (Down x.priority, Down x.status, x.taskId))

    defSortRev t1 t2 = case defSort t1 t2 of
      EQ -> EQ
      LT -> GT
      GT -> LT

sortSomeTasks ::
  -- | Partition completed tasks?
  Bool ->
  -- | Reverse?
  RevSort ->
  -- | Comparison function.
  (SomeTask -> SomeTask -> Ordering) ->
  List SomeTask ->
  List SomeTask
sortSomeTasks partitionCompleted revSort c xs =
  if partitionCompleted
    then
      if revSort ^. #boolIso
        then sortFn completedTasks <> sortFn incompleteTasks
        else sortFn incompleteTasks <> sortFn completedTasks
    else sortFn xs
  where
    sortFn :: List SomeTask -> List SomeTask
    sortFn = fmap (sortSomeTaskSubtasks partitionCompleted revSort c) . L.sortBy c

    (completedTasks, incompleteTasks) = L.partition Task.someTaskIsCompleted xs

sortSomeTaskSubtasks :: Bool -> RevSort -> (SomeTask -> SomeTask -> Ordering) -> SomeTask -> SomeTask
sortSomeTaskSubtasks _ _ _ t@(SomeTaskSingle _) = t
sortSomeTaskSubtasks partitionCompleted revSort c (SomeTaskGroup t) =
  SomeTaskGroup $ sortTaskGroupSubtasks partitionCompleted revSort c t

sortTaskGroupSubtasks :: Bool -> RevSort -> (SomeTask -> SomeTask -> Ordering) -> TaskGroup -> TaskGroup
sortTaskGroupSubtasks partitionCompleted revSort c t = t {subtasks = subtasks'}
  where
    -- TODO: Avoid the Seq <-> List conversion (Either replace a type or
    -- use Foldable).
    subtasks' =
      sortSomeTasks partitionCompleted revSort c (toList t.subtasks)

cSomeTask :: (Ord a) => (SomeTask -> a) -> SomeTask -> SomeTask -> Ordering
cSomeTask f x y = f x `compare` f y

traverseSorted :: (SingleTask -> a) -> (TaskGroup -> a) -> SortedTasks -> List a
traverseSorted f g = traverseSomeTasks f g . (.unSortedTasks)

parseSortType :: (MonadFail m) => m Text -> m SortType
parseSortType mTxt =
  mTxt >>= \case
    "priority" -> pure SortPriority
    "status" -> pure SortStatus
    "priority_status" -> pure SortPriorityStatus
    "status_priority" -> pure SortStatusPriority
    other -> fail $ "Unexpected sort: " <> unpack other
