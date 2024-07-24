module Todo.Data.Task.Optics
  ( -- * Traversals
    someTaskStatusATraversal,
    someTaskTraversal,
    someTaskPredTraversal,
    taskGroupTraversal,

    -- * Other
    someTaskIdStatusGetter,
  )
where

import Todo.Data.Task
  ( SingleTask (status),
    SomeTask (SomeTaskGroup, SomeTaskSingle),
    TaskGroup (status, subtasks),
    _SomeTaskGroup,
  )
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskStatus (TaskStatus)
import Todo.Prelude

someTaskStatusATraversal :: AffineTraversal' SomeTask TaskStatus
someTaskStatusATraversal = atraversal getter setter
  where
    getter (SomeTaskSingle t) = Right t.status
    getter st@(SomeTaskGroup tg) = case tg.status of
      Just s -> Right s
      Nothing -> Left st

    setter st status = set' #status status st

-- | Traverses all tasks.
someTaskTraversal :: Traversal' SomeTask SomeTask
someTaskTraversal = someTaskPredTraversal (const True)

-- | Traverses all task groups.
taskGroupTraversal :: Traversal' SomeTask SomeTask
taskGroupTraversal = someTaskPredTraversal (is _SomeTaskGroup)

-- | Traverses all tasks that satisfy the predicate.
someTaskPredTraversal :: (SomeTask -> Bool) -> Traversal' SomeTask SomeTask
someTaskPredTraversal pred = traversalVL f
  where
    f :: forall f. (Applicative f) => (SomeTask -> f SomeTask) -> SomeTask -> f SomeTask
    f g = go
      where
        go :: SomeTask -> f SomeTask
        go st@(SomeTaskSingle _) = appyIfPred st
        go st@(SomeTaskGroup tg) =
          -- Run the effectful fn over all subtasks and the group itself.
          let stA = appyIfPred st
              subtasksA = traverse go tg.subtasks
           in -- NOTE: It is important that stA is traversed before subtasksA
              -- to keep order intact.
              flip (set' subtasksATraversal) <$> stA <*> subtasksA

        subtasksATraversal :: AffineTraversal' SomeTask (Seq SomeTask)
        subtasksATraversal = _SomeTaskGroup % #subtasks

        appyIfPred :: SomeTask -> f SomeTask
        appyIfPred st =
          if pred st
            then g st
            else pure st

someTaskIdStatusGetter :: Getter SomeTask (Tuple2 TaskId TaskStatus)
someTaskIdStatusGetter = to $ \t -> (t ^. #taskId, t ^. #status)
