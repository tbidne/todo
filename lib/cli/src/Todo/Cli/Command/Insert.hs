{- HLINT ignore "Redundant bracket" -}

-- NOTE: HLINT ignore due to OverloadedRecordDot false positives e.g.
-- (some thing).field.

module Todo.Cli.Command.Insert
  ( insertTask,
  )
where

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Effects.Time (MonadTime (getSystemZonedTime))
import Todo.Cli.Command.Utils qualified as CUtils
import Todo.Cli.Configuration.Core
  ( CoreConfig
      ( colorSwitch,
        index,
        unicodeSwitch
      ),
    CoreConfigMerged,
  )
import Todo.Cli.Prelude
import Todo.Cli.Render qualified as Render
import Todo.Cli.Render.TaskId qualified as RTaskId
import Todo.Cli.Render.Utils (ColorSwitch)
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
import Todo.Data.Task.Optics qualified as TaskO
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Data.TaskPriority (TaskPriority (Normal))
import Todo.Data.TaskPriority qualified as TaskPriority
import Todo.Data.TaskStatus (TaskStatus (NotStarted))
import Todo.Data.TaskStatus qualified as TaskStatus
import Todo.Data.Timestamp qualified as Timestamp
import Todo.Exception (DuplicateIdE (MkDuplicateIdE))
import Todo.Index (GroupTaskId, Index, Index𝕌, Index𝕍, (∈))
import Todo.Index qualified as Index
import Todo.Index.Optics qualified as IndexO
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
  let indexUnverified = Index.unverify index
  (newIndex, newTaskIds) <-
    Utils.whileApplySetM indexUnverified getMoreTasksAns (mkSomeTask color)

  indexVerified <- Index.verify newIndex

  if null newTaskIds
    then
      putTextLn "Did not add any tasks."
    else do
      Index.writeIndex indexVerified

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
    index :: Index𝕍
    index = coreConfig.index
    unicode = coreConfig.unicodeSwitch

mkSomeTask ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  ColorSwitch ->
  Index s ->
  m (Index𝕌, TaskId)
mkSomeTask color index = do
  let indexToGroupIds =
        IndexO.unverifyGetter
          % IndexO.indexTraversal
          % TaskO.taskGroupTraversal
          % #taskId

      parentIds = toListOf indexToGroupIds index

  putTextLn "Parent id(s):\n"

  for_ parentIds $ \i -> do
    let idRendered = RTaskId.render color i
    putTextLn $ "- " <> builderToTxt idRendered
  putTextLn ""

  mParentId <-
    getExtantTaskGroupIdOrEmpty
      "Task id for parent group (leave blank for no parent)? "
      index

  shouldMkTaskGroup <- CUtils.askYesNoQ "Create (empty) task group (y/n)? "

  newTask <-
    if shouldMkTaskGroup
      then SomeTaskGroup <$> mkTaskGroup index
      else SomeTaskSingle <$> mkOneTask index

  let newIndex = case mParentId of
        Nothing -> Index.insert newTask index
        Just parentId -> Index.insertAtTaskId parentId newTask index

  pure (newIndex, newTask.taskId)

-- | Like 'mkOneTask', except we create an empty task group.
mkTaskGroup ::
  forall m s.
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Index s ->
  m TaskGroup
mkTaskGroup index = do
  -- see NOTE: [Id prefix newline]
  taskId <- getTaskId "\nGroup id: " index

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

  pure
    $ MkTaskGroup
      { priority,
        status,
        taskId = taskId,
        subtasks = Empty
      }

-- | Makes a single task. If the input is simply the index (i.e. we are
-- inserting a top-level task), returns simply the new task to be inserted
-- (Left). If the input is the index + group TaskId, then we return
-- new Task + group TaskId.
mkOneTask ::
  forall m s.
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Index s ->
  m SingleTask
mkOneTask index = do
  -- NOTE: [Id prefix newline]
  --
  -- We prefix a newline here to start of the "task data" group i.e. separate
  -- it from the previous question, which was "should we create an empty
  -- task group".
  --
  -- We don't print the newline right after there so that if this fails
  -- (thus triggering a retry), we get newlines inbetween attempts, which
  -- makes for nicer reading.
  --
  -- It might be nicer to put the newline after the failed attempt, however.
  taskId <- getTaskId "\nId: " index

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

  pure
    $ MkSingleTask
      { deadline,
        description,
        priority,
        status,
        taskId = taskId
      }

-- | Retrieves a TaskId guaranteed to not be in the Index.
getTaskId ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  -- | Text prompt.
  Text ->
  -- | Index.
  Index s ->
  -- | TaskId guaranteed to not be in the index.
  m TaskId
getTaskId qsn index = go
  where
    go = do
      idTxt <- CUtils.getStrippedLine qsn
      case TaskId.parseTaskId idTxt of
        EitherLeft err -> do
          putTextLn $ CUtils.formatBadResponse err
          go
        EitherRight taskId ->
          if taskId ∈ index
            then do
              putTextLn $ displayExceptiont $ MkDuplicateIdE taskId
              go
            else pure taskId

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
  Index s ->
  -- | The group task id, if the user asked for it.
  m (Maybe GroupTaskId)
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
            EitherRight taskId ->
              case Index.findGroupTaskId taskId index of
                Left err -> putTextLn err *> go
                Right x -> pure $ Just x

getMoreTasksAns ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  m Bool
getMoreTasksAns = CUtils.askYesNoQ "\nCreate a task (y/n)? "
