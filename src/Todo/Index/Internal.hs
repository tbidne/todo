{-# LANGUAGE UndecidableInstances #-}

module Todo.Index.Internal
  ( Index (..),
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

-- | Todo index.
data Index = UnsafeIndex (List SomeTask) OsPath
  deriving stock (Eq, Show)

instance HasField "taskList" Index (List SomeTask) where
  getField (UnsafeIndex tl _) = tl

instance HasField "path" Index OsPath where
  getField (UnsafeIndex _ p) = p

instance
  (k ~ A_Lens, a ~ List SomeTask, b ~ List SomeTask) =>
  LabelOptic "taskList" k Index Index a b
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
  LabelOptic "path" k Index Index a b
  where
  labelOptic =
    lensVL
      $ \f
         (UnsafeIndex _taskList _path) ->
          fmap
            (UnsafeIndex _taskList)
            (f _path)
  {-# INLINE labelOptic #-}

type instance At.Index Index = TaskId

type instance IxValue Index = SomeTask

instance Ixed Index where
  type IxKind Index = An_AffineTraversal

  ix :: TaskId -> AffineTraversal' Index SomeTask
  ix taskId =
    atraversal
      (\idx -> mToE idx $ lookup taskId idx)
      (\idx -> replaceAtId taskId idx . Just)
  {-# INLINE ix #-}

instance At Index where
  at :: TaskId -> Lens' Index (Maybe SomeTask)
  at taskId = lens (lookup taskId) (replaceAtId taskId)
  {-# INLINE at #-}

-- | Looks up the TaskId in the Index.
lookup :: TaskId -> Index -> Maybe SomeTask
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
replaceAtId :: TaskId -> Index -> Maybe SomeTask -> Index
replaceAtId taskId (UnsafeIndex taskList path) mNewTask =
  (`UnsafeIndex` path) $ foldr go [] taskList
  where
    go :: SomeTask -> List SomeTask -> List SomeTask
    go st@(SomeTaskSingle t) acc =
      if t.taskId == taskId
        then prependNewTask acc
        else st : acc
    go (SomeTaskGroup tg) acc =
      if tg.taskId == taskId
        then prependNewTask acc
        else
          -- TODO: This will techincally replace all with the matching
          -- ids, though it should be fine as task ids should be unique.
          -- Maybe we could improve this.
          let subtasks' = ((\xs -> listToSeq (go xs [])) =<< tg.subtasks)
           in SomeTaskGroup (tg {subtasks = subtasks'}) : acc

    prependNewTask = case mNewTask of
      Just newTask -> (newTask :)
      Nothing -> identity
