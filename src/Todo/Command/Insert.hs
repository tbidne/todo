{- HLINT ignore "Redundant bracket" -}

-- NOTE: HLINT ignore due to OverloadedRecordDot false positives e.g.
-- (some thing).field.

module Todo.Command.Insert
  ( insertTask,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Bitraversable (bitraverse)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Effects.Time (MonadTime (getSystemZonedTime))
import Refined (Refined)
import Refined qualified as R
import Refined.Extras qualified as RE
import Todo.Command.Utils qualified as CUtils
import Todo.Configuration.Core
  ( CoreConfig
      ( colorSwitch,
        index,
        unicodeSwitch
      ),
    CoreConfigMerged,
  )
import Todo.Configuration.Default (Default (def))
import Todo.Data.Sorted qualified as Sorted
import Todo.Data.Task
  ( SingleTask
      ( MkSingleTask,
        deadline,
        description,
        priority,
        status,
        taskId
      ),
    SomeTask (SomeTaskGroup, SomeTaskSingle),
    TaskGroup (MkTaskGroup, priority, status, subtasks, taskId),
  )
import Todo.Data.Task qualified as Task
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Data.TaskPriority (TaskPriority (Normal))
import Todo.Data.TaskPriority qualified as TaskPriority
import Todo.Data.TaskStatus (TaskStatus (NotStarted))
import Todo.Data.TaskStatus qualified as TaskStatus
import Todo.Data.Timestamp qualified as Timestamp
import Todo.Index (Index)
import Todo.Index qualified as Index
import Todo.Index.Safe
  ( GroupIdMember,
    GroupTaskId (MkGroupTaskId),
    IndexWithData (MkIndexWithData),
    RIndexParentId,
    RIndexTask,
    RIndexTaskId,
    RIndexTaskIdParentId,
    RIndexTaskParentId,
    SingleTaskId (MkSingleTaskId),
  )
import Todo.Index.Safe qualified as Safe
import Todo.Prelude
import Todo.Render qualified as Render
import Todo.Render.Utils (ColorSwitch)
import Todo.Utils qualified as Utils

-- | Inserts new task(s) into the file.
insertTask ::
  forall m.
  ( HasCallStack,
    MonadFail m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadTerminal m,
    MonadTime m,
    MonadThrow m
  ) =>
  CoreConfigMerged ->
  m ()
insertTask coreConfig = do
  (newIndex, newTaskIds) <-
    Utils.whileApplySetM index getMoreTasksAns (mkSomeTask color)

  if null newTaskIds
    then
      putTextLn "Did not add any tasks."
    else do
      Index.writeIndex newIndex

      currTime <- getSystemZonedTime

      let indexDiff = Index.filterOnIds newTaskIds newIndex
          sorted = Sorted.sortTasks Nothing def (snd $ Index.toList indexDiff)

      putTextLn "Successfully added task. Modified tasks:\n"
      putTextLn
        $ TL.toStrict
        $ TLB.toLazyText
        $ Render.renderSorted currTime color unicode sorted
  where
    color = coreConfig.colorSwitch
    index = coreConfig.index
    unicode = coreConfig.unicodeSwitch

mkSomeTask ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  ColorSwitch ->
  Index ->
  m (Index, TaskId)
mkSomeTask color index = do
  let indexToGroupIds =
        Index.indexTraversal
          % Task.taskGroupTraversal
          % #taskId

      parentIds = toListOf indexToGroupIds index

  putTextLn "Parent id(s):\n"

  for_ parentIds $ \i -> do
    let idRendered = TaskId.render color i
    putTextLn $ "- " <> builderToTxt idRendered
  putTextLn ""

  mIndexParentId <-
    getExtantTaskGroupIdOrEmpty
      "Task id for parent group (leave blank for no parent)? "
      index

  let eIndexMaybeParentId :: Either Index RIndexParentId
      eIndexMaybeParentId = case mIndexParentId of
        Nothing -> Left index
        Just parentTaskId -> Right parentTaskId

  shouldMkTaskGroup <- CUtils.askYesNoQ "Create (empty) task group (y/n)? "

  eIndexTaskMaybeParentId <-
    if shouldMkTaskGroup
      then mkTaskGroup eIndexMaybeParentId
      else mkOneTask eIndexMaybeParentId

  let (newIndex, taskId) = case eIndexTaskMaybeParentId of
        Left rIndexNewTask ->
          ( Safe.insert rIndexNewTask,
            (R.unrefine rIndexNewTask).taskId
          )
        Right rIndexNewTaskAndGroupId ->
          ( Safe.insertAtGroupId rIndexNewTaskAndGroupId,
            (R.unrefine rIndexNewTaskAndGroupId).taskId
          )

  pure (newIndex, taskId)

-- | Like 'mkOneTask', except we create an empty task group.
mkTaskGroup ::
  forall m.
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Either Index RIndexParentId ->
  m (Either RIndexTask RIndexTaskParentId)
mkTaskGroup eIndexMaybeParentId = do
  eMkTask <- bitraverse mkTask mkTaskWithParentId eIndexMaybeParentId

  let statusPrompt =
        mconcat
          [ "Status ",
            TaskStatus.metavar,
            " (leave blank for none): "
          ]
      priorityPrompt =
        mconcat
          [ "Priority ",
            TaskPriority.metavar,
            " (leave blank for none): "
          ]

  status <- CUtils.askParseEmptyQ statusPrompt TaskStatus.parseTaskStatus

  priority <- CUtils.askParseEmptyQ priorityPrompt TaskPriority.parseTaskPriority

  -- see NOTE: [Redundant bimap]
  pure
    $ bimap
      (\f -> f (priority, status))
      (\f -> f (priority, status))
      eMkTask
  where
    mkTask index =
      indexToTask "Group id: " index $ \(priority, status) tid ->
        SomeTaskGroup
          $ MkTaskGroup
            { priority,
              status,
              taskId = tid,
              subtasks = Empty
            }
    mkTaskWithParentId rIndexGroupId =
      indexGroupIdToTask @m rIndexGroupId $ \(priority, status) tid ->
        SomeTaskGroup
          $ MkTaskGroup
            { priority,
              status,
              taskId = tid,
              subtasks = Empty
            }

-- | Makes a single task. If the input is simply the index (i.e. we are
-- inserting a top-level task), returns simply the new task to be inserted
-- (Left). If the input is the index + group TaskId, then we return
-- new Task + group TaskId.
mkOneTask ::
  forall m.
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Either Index RIndexParentId ->
  m (Either RIndexTask RIndexTaskParentId)
mkOneTask eIndexMaybeParentId = do
  eMkTask <- bitraverse mkTask mkTaskWithParentId eIndexMaybeParentId

  let statusPrompt =
        mconcat
          [ "Status ",
            TaskStatus.metavar,
            " (leave blank for not-started): "
          ]
      priorityPrompt =
        mconcat
          [ "Priority ",
            TaskPriority.metavar,
            " (leave blank for normal): "
          ]

  mStatus <- CUtils.askParseEmptyQ statusPrompt TaskStatus.parseTaskStatus

  let status = fromMaybe NotStarted mStatus

  mPriority <- CUtils.askParseEmptyQ priorityPrompt TaskPriority.parseTaskPriority

  let priority = fromMaybe Normal mPriority

  description <- CUtils.getStrippedLineEmpty "Description (leave blank for none): "

  deadline <-
    CUtils.askParseEmptyQ
      "Deadline (leave blank for none): "
      (Timestamp.parseTimestamp . unpack)

  -- NOTE: [Redundant bimap]
  --
  -- For some reason, we cannot seem to write a single polymorphic
  --
  --     (\f -> f (deadline, description, priority, status))
  --
  -- that we can use for both.
  pure
    $ bimap
      (\f -> f (deadline, description, priority, status))
      (\f -> f (deadline, description, priority, status))
      eMkTask
  where
    mkTask index =
      indexToTask "\nId: " index $ \(deadline, description, priority, status) tid ->
        SomeTaskSingle
          $ MkSingleTask
            { deadline,
              description,
              priority,
              status,
              taskId = tid
            }
    mkTaskWithParentId rIndexGroupId =
      indexGroupIdToTask rIndexGroupId $ \(deadline, description, priority, status) tid ->
        SomeTaskSingle
          $ MkSingleTask
            { deadline,
              description,
              priority,
              status,
              taskId = tid
            }

-- | Uses the index and the given map to SomeTask to create a refined
-- SomeTask.
indexToTask ::
  forall m a.
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Text ->
  Index ->
  (a -> TaskId -> SomeTask) ->
  m (a -> RIndexTask)
indexToTask prompt index onTaskId = do
  indexTaskId <- getTaskId prompt index
  pure $ \extraParams -> Safe.addTaskToId indexTaskId (onTaskId extraParams)

-- | Like 'indexToTask', except we compose the refined SomeTask with the
-- given refined GroupTaskId.
indexGroupIdToTask ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  RIndexParentId ->
  (a -> TaskId -> SomeTask) ->
  m (a -> RIndexTaskParentId)
indexGroupIdToTask indexParentId onTaskId = do
  indexTaskIdParentId <- getTaskIdWithGroupId "\nId: " indexParentId
  pure $ \extraParams -> Safe.addTaskToIdAndGroupId indexTaskIdParentId (onTaskId extraParams)

-- | Retrieves a TaskId guaranteed to be in the Index.
getTaskId ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  -- | Text prompt.
  Text ->
  -- | Index.
  Index ->
  -- | TaskId guaranteed to be in the index.
  m RIndexTaskId
getTaskId qsn index = go
  where
    go = do
      idTxt <- CUtils.getStrippedLine qsn
      case TaskId.parseTaskId idTxt of
        EitherLeft err -> do
          putTextLn $ CUtils.formatBadResponse err
          go
        EitherRight taskId -> do
          case R.refine (MkIndexWithData index (MkSingleTaskId taskId)) of
            Left ex -> do
              putTextLn $ displayRefineException' ex
              go
            Right indexTaskId -> pure indexTaskId

-- | Like 'getTaskId', except it composed the refined TaskId with the refined
-- group TaskId.
getTaskIdWithGroupId ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  -- | Text prompt.
  Text ->
  -- | Refined GroupTaskId.
  RIndexParentId ->
  -- | GroupTaskId g and TaskId t s.t. g is in the index and t is __not__ in the index.
  m RIndexTaskIdParentId
getTaskIdWithGroupId qsn indexParentId = go
  where
    go = do
      idTxt <- CUtils.getStrippedLine qsn
      case TaskId.parseTaskId idTxt of
        EitherLeft err -> do
          putTextLn $ CUtils.formatBadResponse err
          go
        EitherRight taskId -> do
          -- withTaskId is the result of adding taskId to our RIndexParentId,
          -- but before we refine it to an RIndexTaskIdParentId
          let withTaskId :: Refined GroupIdMember (IndexWithData (SingleTaskId, GroupTaskId))
              withTaskId = RE.reallyUnsafeLiftR (fmap (MkSingleTaskId taskId,)) indexParentId
          case R.refine withTaskId of
            Left ex -> do
              putTextLn $ displayRefineException' ex
              go
            Right indexTaskIdParentId -> pure $ joinRefined indexTaskIdParentId

-- | Retrieves a group TaskId guaranteed to be in the Index, or Nothing.
getExtantTaskGroupIdOrEmpty ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  -- | Text prompt
  Text ->
  -- | Index
  Index ->
  -- | Index and group TaskId in the Index, or Nothing.
  m (Maybe RIndexParentId)
getExtantTaskGroupIdOrEmpty qsn index = go
  where
    go = do
      mIdTxt <- CUtils.getStrippedLineEmpty qsn
      case mIdTxt of
        Nothing -> pure Nothing
        Just idTxt ->
          case TaskId.parseTaskId idTxt of
            EitherLeft err -> do
              putTextLn $ CUtils.formatBadResponse err
              go
            EitherRight taskId -> do
              case R.refine @GroupIdMember (MkIndexWithData index (MkGroupTaskId taskId)) of
                Right indexParentId -> pure $ Just indexParentId
                Left ex -> do
                  putTextLn $ displayRefineException' ex
                  go

getMoreTasksAns ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  m Bool
getMoreTasksAns = CUtils.askYesNoQ "\nCreate a task (y/n)? "
