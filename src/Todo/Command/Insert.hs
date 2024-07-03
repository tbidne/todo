module Todo.Command.Insert
  ( insertTask,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Bitraversable (bitraverse)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Effects.FileSystem.HandleWriter (MonadHandleWriter)
import Effects.Time (MonadTime (getSystemZonedTime))
import Refined (Refined, type (&&))
import Refined qualified as R
import Refined.Extras qualified as RE
import Todo.Command.Utils qualified as CUtils
import Todo.Data.Sorted qualified as Sorted
import Todo.Data.Task
  ( SomeTask (MultiTask, SingleTask),
    Task
      ( MkTask,
        deadline,
        description,
        priority,
        status,
        taskId
      ),
    TaskGroup (MkTaskGroup, priority, status, subtasks, taskId),
  )
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Data.TaskPriority qualified as TaskPriority
import Todo.Data.TaskStatus qualified as TaskStatus
import Todo.Data.Timestamp qualified as Timestamp
import Todo.Index (Index)
import Todo.Index qualified as Index
import Todo.Index.Safe
  ( GroupIdMember,
    GroupTaskId (MkGroupTaskId),
    IndexWithData (MkIndexWithData),
    SingleTaskId (MkSingleTaskId),
    TaskIdNotMember,
  )
import Todo.Index.Safe qualified as Safe
import Todo.Prelude
import Todo.Render qualified as Render
import Todo.Render.Utils (ColorSwitch, UnicodeSwitch)

-- | Inserts new task(s) into the file.
insertTask ::
  forall m.
  ( HasCallStack,
    MonadFail m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadTerminal m,
    MonadTime m,
    MonadThrow m
  ) =>
  OsPath ->
  -- | Is color enabled.
  ColorSwitch ->
  -- | Is unicode enabled.
  UnicodeSwitch ->
  m ()
insertTask tasksPath color unicode = do
  index <- Index.readIndex tasksPath

  (newIndex, newTaskIds) <-
    CUtils.withNoBuffering $ whileApplySetM index getMoreTasksAns mkSomeTask

  Index.writeIndex tasksPath newIndex

  currTime <- getSystemZonedTime

  let indexDiff = Index.filterOnIds newTaskIds newIndex
      sorted = Sorted.sortTasks Nothing (Index.toList indexDiff)

  putTextLn "Successfully added task. Modified tasks:\n"
  putTextLn
    $ TL.toStrict
    $ TLB.toLazyText
    $ Render.renderSorted currTime color unicode sorted

mkSomeTask ::
  ( HasCallStack,
    MonadTerminal m
  ) =>
  Index ->
  m (Index, TaskId)
mkSomeTask index = do
  mParentTaskId <-
    getExtantTaskGroupIdOrEmpty
      "Task id for parent group (leave blank for no parent)? "
      index

  let eIndexGroupId :: Either Index (Refined GroupIdMember (IndexWithData GroupTaskId))
      eIndexGroupId = case mParentTaskId of
        Nothing -> Left index
        Just parentTaskId -> Right parentTaskId

  shouldMkTaskGroup <- CUtils.askYesNoQ "Create (empty) task group (y/n)? "

  eIndexWithData <-
    if shouldMkTaskGroup
      then mkTaskGroup eIndexGroupId
      else mkOneTask eIndexGroupId

  let (newIndex, taskId) = case eIndexWithData of
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
    MonadTerminal m
  ) =>
  Either Index (Refined GroupIdMember (IndexWithData GroupTaskId)) ->
  m
    ( Either
        (Refined TaskIdNotMember (IndexWithData SomeTask))
        ( Refined
            (GroupIdMember && TaskIdNotMember)
            (IndexWithData (Tuple2 SomeTask GroupTaskId))
        )
    )
mkTaskGroup eIndexGroupId = do
  eMkTask <- bitraverse mkTask mkTaskWithGroupId eIndexGroupId

  status <-
    askParseEmptyQ
      "Task status (leave blank for none): "
      TaskStatus.parseTaskStatus

  priority <-
    askParseEmptyQ
      "Task priority (leave blank for none): "
      TaskPriority.parseTaskPriority

  pure
    $ bimap
      (\f -> f (priority, status))
      (\f -> f (priority, status))
      eMkTask
  where
    mkTask index =
      indexToTask "Task group id: " index $ \(priority, status) tid ->
        MultiTask
          $ MkTaskGroup
            { priority,
              status,
              taskId = tid,
              subtasks = Empty
            }
    mkTaskWithGroupId rIndexGroupId =
      indexGroupIdToTask @m rIndexGroupId $ \(priority, status) tid ->
        MultiTask
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
    MonadTerminal m
  ) =>
  Either Index (Refined GroupIdMember (IndexWithData GroupTaskId)) ->
  m
    ( Either
        (Refined TaskIdNotMember (IndexWithData SomeTask))
        ( Refined
            (GroupIdMember && TaskIdNotMember)
            (IndexWithData (Tuple2 SomeTask GroupTaskId))
        )
    )
mkOneTask eIndexGroupId = do
  eMkTask <- bitraverse mkTask mkTaskWithGroupId eIndexGroupId

  status <-
    askParseQ
      "Task status (completed | in-progress | not-started | blocked: <ids>): "
      TaskStatus.parseTaskStatus

  priority <-
    askParseQ
      "Task priority (low | normal | high): "
      TaskPriority.parseTaskPriority

  putText "Description (leave blank for none): "
  description <- CUtils.getStrippedLineEmpty

  deadline <-
    askParseEmptyQ
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
      indexToTask "\nTask id: " index $ \(deadline, description, priority, status) tid ->
        SingleTask
          $ MkTask
            { deadline,
              description,
              priority,
              status,
              taskId = tid
            }
    mkTaskWithGroupId rIndexGroupId =
      indexGroupIdToTask rIndexGroupId $ \(deadline, description, priority, status) tid ->
        SingleTask
          $ MkTask
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
    MonadTerminal m
  ) =>
  Text ->
  Index ->
  (a -> TaskId -> SomeTask) ->
  m (a -> Refined TaskIdNotMember (IndexWithData SomeTask))
indexToTask prompt index onTaskId = do
  rTaskId <- getTaskId prompt index
  -- pure $ Safe.addTaskToId rTaskId (onTaskId extraParams)
  pure $ \extraParams -> Safe.addTaskToId rTaskId (onTaskId extraParams)

-- | Like 'indexToTask', except we compose the refined SomeTask with the
-- given refined GroupTaskId.
indexGroupIdToTask ::
  ( HasCallStack,
    MonadTerminal m
  ) =>
  Refined GroupIdMember (IndexWithData GroupTaskId) ->
  (a -> TaskId -> SomeTask) ->
  m
    ( a ->
      Refined
        (GroupIdMember && TaskIdNotMember)
        (IndexWithData (Tuple2 SomeTask GroupTaskId))
    )
indexGroupIdToTask rIndexGroupId onTaskId = do
  rIndexNewIdGroupId <- getTaskIdWithGroupId "\nTask id: " rIndexGroupId
  -- pure $ Safe.addTaskToIdAndGroupId rIndexNewIdGroupId onTaskId
  pure $ \extraParams -> Safe.addTaskToIdAndGroupId rIndexNewIdGroupId (onTaskId extraParams)

-- | Retrieves a TaskId guaranteed to be in the Index.
getTaskId ::
  ( HasCallStack,
    MonadTerminal m
  ) =>
  -- | Text prompt.
  Text ->
  -- | Index.
  Index ->
  -- | TaskId guaranteed to be in the index.
  m (Refined TaskIdNotMember (IndexWithData SingleTaskId))
getTaskId qsn index = go
  where
    go = do
      putText qsn
      idTxt <- CUtils.getStrippedLine
      case TaskId.parseTaskId idTxt of
        EitherLeft err -> do
          putTextLn $ "Bad response: " <> pack err
          go
        EitherRight taskId -> do
          case R.refine (MkIndexWithData index (MkSingleTaskId taskId)) of
            Left ex -> do
              putTextLn $ displayRefineException' ex
              go
            Right x -> pure x

-- | Like 'getTaskId', except it composed the refined TaskId with the refined
-- group TaskId.
getTaskIdWithGroupId ::
  ( HasCallStack,
    MonadTerminal m
  ) =>
  -- | Text prompt.
  Text ->
  -- | Refined GroupTaskId.
  Refined GroupIdMember (IndexWithData GroupTaskId) ->
  -- | GroupTaskId g and TaskId t s.t. g is in the index and t is __not__ in the index.
  m (Refined (GroupIdMember && TaskIdNotMember) (IndexWithData (Tuple2 SingleTaskId GroupTaskId)))
getTaskIdWithGroupId qsn rIndexGroupId = go
  where
    go = do
      putText qsn
      idTxt <- CUtils.getStrippedLine
      case TaskId.parseTaskId idTxt of
        EitherLeft err -> do
          putTextLn $ "Bad response: " <> pack err
          go
        EitherRight taskId -> do
          let withTaskId :: Refined GroupIdMember (IndexWithData (SingleTaskId, GroupTaskId))
              withTaskId = RE.reallyUnsafeLiftR (fmap (MkSingleTaskId taskId,)) rIndexGroupId
          case R.refine @TaskIdNotMember withTaskId of
            Left ex -> do
              putTextLn $ displayRefineException' ex
              go
            Right x -> pure $ joinRefined x

-- | Retrieves a group TaskId guaranteed to be in the Index, or Nothing.
getExtantTaskGroupIdOrEmpty ::
  ( HasCallStack,
    MonadTerminal m
  ) =>
  -- | Text prompt
  Text ->
  -- | Index
  Index ->
  -- | Index and group TaskId in the Index, or Nothing.
  m (Maybe (Refined GroupIdMember (IndexWithData GroupTaskId)))
getExtantTaskGroupIdOrEmpty qsn index = go
  where
    go = do
      putText qsn
      mIdTxt <- CUtils.getStrippedLineEmpty
      case mIdTxt of
        Nothing -> pure Nothing
        Just idTxt ->
          case TaskId.parseTaskId idTxt of
            EitherLeft err -> do
              putTextLn $ "Bad response: " <> pack err
              go
            EitherRight taskId -> do
              case R.refine @GroupIdMember (MkIndexWithData index (MkGroupTaskId taskId)) of
                Right x -> pure $ Just x
                Left ex -> do
                  putTextLn $ displayRefineException' ex
                  go

getMoreTasksAns :: (HasCallStack, MonadTerminal m) => m Bool
getMoreTasksAns = CUtils.askYesNoQ "\nCreate a task (y/n)? "

askParseQ ::
  ( HasCallStack,
    MonadTerminal m
  ) =>
  Text ->
  (Text -> EitherString a) ->
  m a
askParseQ qsn parser = go
  where
    go = do
      putText qsn

      txt <- CUtils.getStrippedLine
      case parser txt of
        EitherLeft err -> do
          putTextLn $ "Bad response: " <> pack err
          go
        EitherRight x -> pure x

askParseEmptyQ ::
  ( HasCallStack,
    MonadTerminal m
  ) =>
  Text ->
  (Text -> EitherString a) ->
  m (Maybe a)
askParseEmptyQ qsn parser = go
  where
    go = do
      putText qsn

      CUtils.getStrippedLineEmpty >>= \case
        Nothing -> pure Nothing
        Just txt ->
          case parser txt of
            EitherLeft err -> do
              putTextLn $ "Bad answer: " <> pack err
              go
            EitherRight x -> pure $ Just x
