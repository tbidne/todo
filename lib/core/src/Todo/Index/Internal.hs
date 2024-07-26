{-# LANGUAGE UndecidableInstances #-}

module Todo.Index.Internal
  ( Index (..),
    IndexState (..),
    IndexUnverified,
    IndexVerified,
    lookup,
    replaceAtId,
  )
where

import Optics.At.Core (At, IxValue, Ixed)
import Optics.At.Core qualified as At
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

type IndexUnverified = Index IndexStateUnverified

type IndexVerified = Index IndexStateVerified

-- | Todo index.
type Index :: IndexState -> Type
data Index s = UnsafeIndex (Seq SomeTask) OsPath
  deriving stock (Eq, Show)

instance HasField "taskList" (Index s) (Seq SomeTask) where
  getField (UnsafeIndex tl _) = tl

instance HasField "path" (Index s) OsPath where
  getField (UnsafeIndex _ p) = p

instance
  (k ~ A_Lens, a ~ Seq SomeTask, b ~ Seq SomeTask) =>
  LabelOptic "taskList" k (Index s) (Index s) a b
  where
  labelOptic =
    lensVL
      $ \f
         (UnsafeIndex _taskList _path) ->
          fmap
            (`UnsafeIndex` _path)
            (f _taskList)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ OsPath, b ~ OsPath) =>
  LabelOptic "path" k (Index s) (Index s) a b
  where
  labelOptic =
    lensVL
      $ \f
         (UnsafeIndex _taskList _path) ->
          fmap
            (UnsafeIndex _taskList)
            (f _path)
  {-# INLINE labelOptic #-}

type instance At.Index (Index _) = TaskId

type instance IxValue (Index _) = SomeTask

instance Ixed (Index s) where
  type IxKind (Index s) = An_AffineTraversal

  ix :: TaskId -> AffineTraversal' (Index s) SomeTask
  ix taskId =
    atraversal
      (\idx -> mToE idx $ lookup taskId idx)
      (\idx -> replaceAtId taskId idx . Just)
  {-# INLINE ix #-}

instance At (Index s) where
  at :: TaskId -> Lens' (Index s) (Maybe SomeTask)
  at taskId = lens (lookup taskId) (replaceAtId taskId)
  {-# INLINE at #-}

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
replaceAtId :: TaskId -> Index s -> Maybe SomeTask -> Index s
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
