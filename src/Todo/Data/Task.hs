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

    -- ** Optics
    _SomeTaskSingle,
    _SomeTaskGroup,
    someTaskStatusATraversal,
    someTaskTraversal,
    someTaskPredTraversal,
    taskGroupTraversal,
  )
where

import Data.Aeson ((.:), (.:?), (.=))
import Data.Aeson qualified as Asn
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Object, Parser)
import Data.Set qualified as Set
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskPriority (TaskPriority (Normal))
import Todo.Data.TaskStatus (TaskStatus (Completed), isCompleted)
import Todo.Data.Timestamp (Timestamp)
import Todo.Prelude
import Todo.Utils qualified as Utils

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
  validateTaskKeys v
  pure
    $ MkSingleTask
      { deadline,
        description,
        priority,
        status,
        taskId
      }

validateTaskKeys :: Object -> Parser ()
validateTaskKeys =
  Utils.validateKeys
    ( Set.fromList
        [ "deadline",
          "description",
          "id",
          "priority",
          "status"
        ]
    )

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

instance
  (k ~ A_Lens, a ~ Maybe Timestamp, b ~ Maybe Timestamp) =>
  LabelOptic "deadline" k SingleTask SingleTask a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkSingleTask _deadline _description _priority _status _taskId) ->
          fmap
            (\deadline' -> MkSingleTask deadline' _description _priority _status _taskId)
            (f _deadline)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe Text, b ~ Maybe Text) =>
  LabelOptic "description" k SingleTask SingleTask a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkSingleTask _deadline _description _priority _status _taskId) ->
          fmap
            (\description' -> MkSingleTask _deadline description' _priority _status _taskId)
            (f _description)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ TaskPriority, b ~ TaskPriority) =>
  LabelOptic "priority" k SingleTask SingleTask a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkSingleTask _deadline _description _priority _status _taskId) ->
          fmap
            (\priority' -> MkSingleTask _deadline _description priority' _status _taskId)
            (f _priority)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ TaskStatus, b ~ TaskStatus) =>
  LabelOptic "status" k SingleTask SingleTask a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkSingleTask _deadline _description _priority _status _taskId) ->
          fmap
            (\status' -> MkSingleTask _deadline _description _priority status' _taskId)
            (f _status)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ TaskId, b ~ TaskId) =>
  LabelOptic "taskId" k SingleTask SingleTask a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkSingleTask _deadline _description _priority _status _taskId) ->
          fmap
            (MkSingleTask _deadline _description _priority _status)
            (f _taskId)
  {-# INLINE labelOptic #-}

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
  validateTaskGroupKeys v
  pure
    $ MkTaskGroup
      { priority,
        status,
        subtasks,
        taskId
      }

validateTaskGroupKeys :: Object -> Parser ()
validateTaskGroupKeys =
  Utils.validateKeys
    ( Set.fromList
        [ "id",
          "priority",
          "status",
          "subtasks"
        ]
    )

instance ToJSON TaskGroup where
  toJSON t =
    Asn.object
      $ stripNulls
        [ "id" .= t.taskId,
          "priority" .= t.priority,
          "status" .= t.status,
          "subtasks" .= t.subtasks
        ]

instance
  (k ~ A_Lens, a ~ Maybe TaskPriority, b ~ Maybe TaskPriority) =>
  LabelOptic "priority" k TaskGroup TaskGroup a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkTaskGroup _priority _status _subtasks _taskId) ->
          fmap
            (\priority' -> MkTaskGroup priority' _status _subtasks _taskId)
            (f _priority)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe TaskStatus, b ~ Maybe TaskStatus) =>
  LabelOptic "status" k TaskGroup TaskGroup a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkTaskGroup _priority _status _subtasks _taskId) ->
          fmap
            (\status' -> MkTaskGroup _priority status' _subtasks _taskId)
            (f _status)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Seq SomeTask, b ~ Seq SomeTask) =>
  LabelOptic "subtasks" k TaskGroup TaskGroup a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkTaskGroup _priority _status _subtasks _taskId) ->
          fmap
            (\subtasks' -> MkTaskGroup _priority _status subtasks' _taskId)
            (f _subtasks)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ TaskId, b ~ TaskId) =>
  LabelOptic "taskId" k TaskGroup TaskGroup a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkTaskGroup _priority _status _subtasks _taskId) ->
          fmap
            (MkTaskGroup _priority _status _subtasks)
            (f _taskId)
  {-# INLINE labelOptic #-}

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

instance
  (k ~ A_Lens, a ~ TaskPriority, b ~ TaskPriority) =>
  LabelOptic "priority" k SomeTask SomeTask a b
  where
  labelOptic = lens getter setter
    where
      getter (SomeTaskSingle t) = t.priority
      getter (SomeTaskGroup tg) = taskGroupPriority tg

      setter (SomeTaskSingle t) newPriority =
        SomeTaskSingle $ set' #priority newPriority t
      setter (SomeTaskGroup tg) newPriority =
        SomeTaskGroup $ set' #priority (Just newPriority) tg
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ TaskStatus, b ~ TaskStatus) =>
  LabelOptic "status" k SomeTask SomeTask a b
  where
  labelOptic = lens getter setter
    where
      getter (SomeTaskSingle t) = t.status
      getter (SomeTaskGroup tg) = taskGroupStatus tg

      setter (SomeTaskSingle t) newStatus =
        SomeTaskSingle $ set' #status newStatus t
      setter (SomeTaskGroup tg) newStatus =
        SomeTaskGroup $ set' #status (Just newStatus) tg
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ TaskId, b ~ TaskId) =>
  LabelOptic "taskId" k SomeTask SomeTask a b
  where
  labelOptic = lens getter setter
    where
      getter (SomeTaskSingle t) = t.taskId
      getter (SomeTaskGroup tg) = tg.taskId

      setter (SomeTaskSingle t) newTaskId =
        SomeTaskSingle $ set' #taskId newTaskId t
      setter (SomeTaskGroup tg) newTaskId =
        SomeTaskGroup $ set' #taskId newTaskId tg
  {-# INLINE labelOptic #-}

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

_SomeTaskSingle :: Prism' SomeTask SingleTask
_SomeTaskSingle =
  prism
    SomeTaskSingle
    ( \case
        SomeTaskSingle t -> Right t
        other -> Left other
    )
{-# INLINE _SomeTaskSingle #-}

_SomeTaskGroup :: Prism' SomeTask TaskGroup
_SomeTaskGroup =
  prism
    SomeTaskGroup
    ( \case
        SomeTaskGroup t -> Right t
        other -> Left other
    )
{-# INLINE _SomeTaskGroup #-}

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
