{-# LANGUAGE UndecidableInstances #-}

module Todo.Data.Task
  ( -- * Main type
    Task (..),

    -- ** Grouped tasks
    TaskGroup (..),
    taskGroupPriority,
    taskGroupStatus,

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
import GHC.Records (HasField (getField))
import Todo.Data.Task.TaskId (TaskId)
import Todo.Data.Task.TaskPriority (TaskPriority)
import Todo.Data.Task.TaskStatus (TaskStatus, isCompleted)
import Todo.Data.Timestamp (Timestamp)
import Todo.Prelude

-- | Task data.
data Task = MkTask
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

instance FromJSON Task where
  parseJSON = Asn.withObject "Task" parseTaskObj

parseTaskObj :: Object -> Parser Task
parseTaskObj v = do
  deadline <- v .:? "deadline"
  description <- v .:? "description"
  taskId <- v .: "id"
  priority <- v .: "priority"
  status <- v .: "status"
  pure
    $ MkTask
      { deadline,
        description,
        priority,
        status,
        taskId
      }

instance ToJSON Task where
  toJSON t =
    Asn.object
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
    subtasks :: NESeq SomeTask,
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
  Nothing -> deriveStatus tg.subtasks
    where
      deriveStatus =
        sconcat
          . fmap (.status)
          . toNonEmpty

-- | Takes either the priority (if it is set), or the greatest priority of its
-- subtasks.
taskGroupPriority :: TaskGroup -> TaskPriority
taskGroupPriority tg = case tg.priority of
  Just p -> p
  Nothing -> derivePriority tg.subtasks
  where
    -- Like deriveStatus, except we filter out complete tasks, because we
    -- don't want e.g. a TaskGroup to derive a High priority if its only
    -- priority High subtask is already completed.
    --
    -- This forces us to use mconcat, since we may filter out all tasks.
    -- That's okay though, since Priority's identity is Low, which is what we
    -- want to default to.
    derivePriority =
      mconcat
        . fmap (.priority)
        . filter (not . someTaskIsCompleted)
        . toList

-- | Wrapper for either a single 'Task' or 'TaskGroup'.
data SomeTask
  = SingleTask Task
  | MultiTask TaskGroup
  deriving stock (Eq, Show)

instance HasField "priority" SomeTask TaskPriority where
  getField (SingleTask t) = t.priority
  getField (MultiTask tg) = taskGroupPriority tg

instance HasField "status" SomeTask TaskStatus where
  getField (SingleTask t) = t.status
  getField (MultiTask tg) = taskGroupStatus tg

instance HasField "taskId" SomeTask TaskId where
  getField (SingleTask t) = t.taskId
  getField (MultiTask tg) = tg.taskId

instance FromJSON SomeTask where
  parseJSON = Asn.withObject "SomeTask" $ \v -> do
    if KM.member "subtasks" v
      then parseMulti v
      else parseSingle v
    where
      parseSingle :: Object -> Parser SomeTask
      parseSingle = fmap SingleTask . parseTaskObj

      parseMulti :: Object -> Parser SomeTask
      parseMulti = fmap MultiTask . parseTaskGroupObj

instance ToJSON SomeTask where
  toJSON (SingleTask t) = toJSON t
  toJSON (MultiTask t) = toJSON t

-- | Returns true iff the task / all subtasks are completed.
someTaskIsCompleted :: SomeTask -> Bool
someTaskIsCompleted st = isCompleted st.status

-- | Traverses a list of 'SomeTask's.
traverseSomeTasks ::
  forall a.
  (Task -> a) ->
  (TaskGroup -> a) ->
  List SomeTask ->
  List a
traverseSomeTasks fromTask fromTaskGroup = (>>= go)
  where
    go :: SomeTask -> List a
    go (SingleTask t) = [fromTask t]
    go (MultiTask t) = fromTaskGroup t : (toList t.subtasks >>= go)
