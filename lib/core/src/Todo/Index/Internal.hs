{-# LANGUAGE UndecidableInstances #-}

module Todo.Index.Internal
  ( -- * Types
    Index (..),
    IndexState (..),
    Index𝕌,
    Index𝕍,

    -- * Misc,
    unverify,
    lookup,
    replaceAtId,
  )
where

import Todo.Data.Task
  ( SingleTask (taskId),
    SomeTask (SomeTaskGroup, SomeTaskSingle),
    TaskGroup (subtasks, taskId),
  )
import Todo.Data.TaskId (TaskId)
import Todo.Prelude

-- | The possible verification states w.r.t internal invariants. These are:
--
-- 1. All task ids unique.
-- 2. All id references exist.
data IndexState
  = -- | Invariants unverified.
    IndexStateUnverified
  | -- | Invariants verified.
    IndexStateVerified
  deriving stock (Eq, Show)

-- | Unverified index. U+1D54C
type Index𝕌 = Index IndexStateUnverified

-- | Verified index. U+1D54D
type Index𝕍 = Index IndexStateVerified

-- Disallow coerce :: Index𝕌 -> Index𝕍.
type role Index nominal

-- | Todo index.
type Index :: IndexState -> Type
data Index s = UnsafeIndex (Seq SomeTask) OsPath
  deriving stock (Eq, Show)

instance HasField "taskList" (Index s) (Seq SomeTask) where
  getField (UnsafeIndex tl _) = tl

instance HasField "path" (Index s) OsPath where
  getField (UnsafeIndex _ p) = p

-- | Looks up the TaskId in the Index.
lookup :: TaskId -> Index s -> Maybe SomeTask
lookup taskId (UnsafeIndex taskList _) = foldMapAlt go taskList
  where
    go (SomeTaskSingle t) =
      if t.taskId == taskId
        then Just $ SomeTaskSingle t
        else Nothing
    go (SomeTaskGroup tg) =
      if tg.taskId == taskId
        then Just $ SomeTaskGroup tg
        else foldMapAlt go tg.subtasks

-- | @replaceAtId taskId index newTask@ replaces all tasks corresponding to
-- @taskId@ in @index@ with @newTask@.
replaceAtId :: TaskId -> Index s -> Maybe SomeTask -> Index𝕌
replaceAtId taskId (UnsafeIndex taskList path) mNewTask =
  (`UnsafeIndex` path) $ foldr go Empty taskList
  where
    go :: SomeTask -> Seq SomeTask -> Seq SomeTask
    go st@(SomeTaskSingle t) acc =
      if t.taskId == taskId
        then prependNewTask acc
        else st :<| acc
    go (SomeTaskGroup tg) acc =
      if tg.taskId == taskId
        then prependNewTask acc
        else
          -- TODO: This will technically replace all with the matching
          -- ids, though it should be fine as task ids should be unique.
          -- Maybe we could improve this.
          let subtasks' = (`go` Empty) =<< tg.subtasks
           in SomeTaskGroup (tg {subtasks = subtasks'}) :<| acc

    prependNewTask = case mNewTask of
      Just newTask -> (newTask :<|)
      Nothing -> identity

-- | Forgets verification status on an Index.
unverify :: Index s -> Index𝕌
unverify (UnsafeIndex taskList path) = UnsafeIndex taskList path
