{-# LANGUAGE UndecidableInstances #-}

module Todo.Data.Task
  ( -- * Main type
    SingleTask (..),

    -- ** Grouped tasks
    TaskGroup (..),
    taskGroupStatus,
    taskGroupPriority,

    -- ** Some Task
    SomeTask (..),
    someTaskIsCompleted,
    traverseSomeTasks,
  )
where

import Data.Aeson ((.:), (.:?), (.=))
import Data.Aeson qualified as Asn
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Object, Parser)
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskPriority (TaskPriority (Normal))
import Todo.Data.TaskStatus (TaskStatus (Completed), isCompleted)
import Todo.Data.Timestamp (Timestamp)
import Todo.Prelude

-- | Task data.
data SingleTask = MkSingleTask
  { -- | Optional deadline.
    deadline :: Maybe Timestamp,
    -- | Optional description.
    description :: Maybe Text,
    -- | Priority.
    priority :: TaskPriority,
    -- | Status.
    status :: TaskStatus,
    -- | Id.
    taskId :: TaskId
  }
  deriving stock (Eq, Show)

instance FromJSON SingleTask where
  parseJSON = Asn.withObject "Task" parseTaskObj

parseTaskObj :: Object -> Parser SingleTask
parseTaskObj v = do
  deadline <- v .:? "deadline"
  description <- v .:? "description"
  taskId <- v .: "id"
  priority <- v .: "priority"
  status <- v .: "status"
  pure
    $ MkSingleTask
      { deadline,
        description,
        priority,
        status,
        taskId
      }

instance ToJSON SingleTask where
  toJSON t =
    Asn.object
      $ stripNulls
        [ "deadline" .= t.deadline,
          "description" .= t.description,
          "id" .= t.taskId,
          "priority" .= t.priority,
          "status" .= t.status
        ]

-- | Multiple tasks.
data TaskGroup = MkTaskGroup
  { -- | Optional priority.
    priority :: Maybe TaskPriority,
    -- | Optional status.
    status :: Maybe TaskStatus,
    -- | List of subtasks.
    subtasks :: Seq SomeTask,
    -- | Id.
    taskId :: TaskId
  }
  deriving stock (Eq, Show)

instance FromJSON TaskGroup where
  parseJSON = Asn.withObject "TaskGroup" parseTaskGroupObj

parseTaskGroupObj :: Object -> Parser TaskGroup
parseTaskGroupObj v = do
  taskId <- v .: "id"
  priority <- v .:? "priority"
  status <- v .:? "status"
  subtasks <- v .: "subtasks"
  pure
    $ MkTaskGroup
      { priority,
        status,
        subtasks,
        taskId
      }

instance ToJSON TaskGroup where
  toJSON t =
    Asn.object
      $ stripNulls
        [ "id" .= t.taskId,
          "priority" .= t.priority,
          "status" .= t.status,
          "subtasks" .= t.subtasks
        ]

-- | Takes either the status (if it is set), or the greatest status of its
-- subtasks.
taskGroupStatus :: TaskGroup -> TaskStatus
taskGroupStatus tg = case tg.status of
  Just s -> s
  Nothing -> case tg.subtasks of
    -- Empty -> Completed
    Empty -> Completed
    -- NonEmpty -> multiply together
    (x :<| xs) -> deriveStatus x.status xs
    where
      -- NOTE: [Deriving empty status]
      --
      -- Why don't we use sconcat + Completed? Because
      --
      --   Completed <> NotStarted == InProgress
      --
      -- which is not what we want. The is exactly what prevents TaskStatus
      -- from being a monoid. OTOH, defaulting empty to Completed and o/w
      -- multiplying all non-empty elements together gets us what we want.
      --
      -- See NOTE: [Deriving empty priority] for a longer exposition on this
      -- problem wrt priority.
      deriveStatus y =
        sconcat
          . (y :|)
          . fmap (.status)
          . toList

taskGroupPriority :: TaskGroup -> TaskPriority
taskGroupPriority tg = case tg.priority of
  Just p -> p
  Nothing -> case tg.subtasks of
    -- Empty -> Normal
    Empty -> Normal
    -- NonEmpty -> multiply together
    (x :<| xs) -> derivePriority x.priority xs
    where
      derivePriority y =
        sconcat
          -- NOTE: [Deriving empty priority]
          --
          -- Notice different cases for empty vs. non-empty. Why don't we use
          -- sconcat or mconcat? Consider that we have:
          --
          --   Normal <> Low == Normal
          --
          -- Because of that, here's what we'd get:
          --
          -- 1. sconcat Low [] -> Low          (should be Normal)
          -- 2. sconcat Normal [Low] -> Normal (should be Low)
          -- 3. mconcat [Low] -> Normal        (should be Low)
          --
          -- We only want to fall back to Normal when there are no statuses
          -- (i.e. subtasks is empty). But each of the strategies above
          -- has at least one scenario where it doesn't give us what we want.
          --
          -- Hence we explicitly split out each cases, which does what we want.
          --
          -- See NOTE: [Deriving empty status] for the same idea, but for
          -- statuses.
          . (y :|)
          . fmap (.priority)
          . filter (not . someTaskIsCompleted)
          . toList

-- | Wrapper for either a single 'Task' or 'TaskGroup'.
data SomeTask
  = SomeTaskSingle SingleTask
  | SomeTaskGroup TaskGroup
  deriving stock (Eq, Show)

instance HasField "priority" SomeTask TaskPriority where
  getField (SomeTaskSingle t) = t.priority
  getField (SomeTaskGroup tg) = taskGroupPriority tg

instance HasField "status" SomeTask TaskStatus where
  getField (SomeTaskSingle t) = t.status
  getField (SomeTaskGroup tg) = taskGroupStatus tg

instance HasField "taskId" SomeTask TaskId where
  getField (SomeTaskSingle t) = t.taskId
  getField (SomeTaskGroup tg) = tg.taskId

instance FromJSON SomeTask where
  parseJSON = Asn.withObject "SomeTask" $ \v -> do
    if KM.member "subtasks" v
      then parseMulti v
      else parseSingle v
    where
      parseSingle :: Object -> Parser SomeTask
      parseSingle = fmap SomeTaskSingle . parseTaskObj

      parseMulti :: Object -> Parser SomeTask
      parseMulti = fmap SomeTaskGroup . parseTaskGroupObj

instance ToJSON SomeTask where
  toJSON (SomeTaskSingle t) = toJSON t
  toJSON (SomeTaskGroup t) = toJSON t

-- | Returns true iff the task / all subtasks are completed.
someTaskIsCompleted :: SomeTask -> Bool
someTaskIsCompleted st = isCompleted st.status

-- | Traverses a list of 'SomeTask's.
traverseSomeTasks ::
  forall a.
  (SingleTask -> a) ->
  (TaskGroup -> a) ->
  List SomeTask ->
  List a
traverseSomeTasks fromTask fromTaskGroup = (>>= go)
  where
    go :: SomeTask -> List a
    go (SomeTaskSingle t) = [fromTask t]
    go (SomeTaskGroup t) = fromTaskGroup t : (toList t.subtasks >>= go)
