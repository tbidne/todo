module Todo
  ( -- * Info
    listTasks,

    -- * Update
    deleteTask,
    insertTask,
  )
where

import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Effects.FileSystem.HandleWriter
  ( BufferMode (LineBuffering, NoBuffering),
    MonadHandleWriter,
  )
import Effects.FileSystem.HandleWriter qualified as HW
import Effects.Time (MonadTime (getSystemZonedTime))
import System.IO qualified as IO
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
import Todo.Data.Task.Render qualified as Render
import Todo.Data.Task.Render.Utils (ColorSwitch, UnicodeSwitch)
import Todo.Data.Task.Sorted (SortType)
import Todo.Data.Task.Sorted qualified as Sorted
import Todo.Data.Task.TaskId (TaskId)
import Todo.Data.Task.TaskId qualified as TaskId
import Todo.Data.Task.TaskPriority qualified as TaskPriority
import Todo.Data.Task.TaskStatus qualified as TaskStatus
import Todo.Data.Timestamp qualified as Timestamp
import Todo.Index (DuplicateIdE (MkDuplicateIdE), Index, (∈))
import Todo.Index qualified as Index
import Todo.Prelude

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
  m ()
insertTask tasksPath = do
  index <- Index.readIndex tasksPath

  noBuffering

  shouldMkTaskGroup <- askYesNoQ "Create task group (y/n)? "

  newTask <-
    if shouldMkTaskGroup
      then do
        taskGroup <- mkTaskGroup index
        pure $ MultiTask taskGroup
      else do
        task <- mkOneTask index
        pure $ SingleTask task

  let index' = Index.reallyUnsafeInsert newTask index

  lineBuffering

  Index.writeIndex tasksPath index'

  rendered <- Render.renderOneNoStyle newTask

  putTextLn "Successfully added task:\n"
  putTextLn $ builderToTxt rendered
  where
    mkTaskGroup :: Index -> m TaskGroup
    mkTaskGroup index = do
      taskId <- getTaskId "Task group id: " index

      status <-
        askParseEmptyQ
          "Task status (leave blank for none): "
          TaskStatus.parseTaskStatus

      priority <-
        askParseEmptyQ
          "Task priority (leave blank for none): "
          TaskPriority.parseTaskPriority

      putTextLn "\nNow enter subtask(s) information"

      subtasks <- whileM getMoreTasksAns (SingleTask <$> mkOneTask index)

      pure
        $ MkTaskGroup
          { taskId,
            priority,
            status,
            subtasks
          }

    mkOneTask :: Index -> m Task
    mkOneTask index = do
      taskId <- getTaskId "\nTask id: " index

      status <-
        askParseQ
          "Task status (completed | in-progress | not-started | blocked: <ids>): "
          TaskStatus.parseTaskStatus

      priority <-
        askParseQ
          "Task priority (low | normal | high): "
          TaskPriority.parseTaskPriority

      putText "Description (leave blank for none): "
      description <- getStrippedLineEmpty

      deadline <-
        askParseEmptyQ
          "Deadline (leave blank for none): "
          (Timestamp.parseTimestamp . unpack)

      pure
        $ MkTask
          { deadline,
            description,
            priority,
            status,
            taskId
          }

    getTaskId :: Text -> Index -> m TaskId
    getTaskId qsn index = go
      where
        go = do
          putText qsn
          idTxt <- getStrippedLine
          case TaskId.parseTaskId idTxt of
            EitherLeft err -> do
              putTextLn $ "Bad response: " <> pack err
              go
            EitherRight taskId -> do
              if taskId ∈ index
                then do
                  putStrLn $ displayException $ MkDuplicateIdE taskId
                  go
                else
                  pure taskId

    getMoreTasksAns :: m Bool
    getMoreTasksAns = askYesNoQ "\nAnother task (y/n)? "

    askParseQ :: Text -> (Text -> EitherString a) -> m a
    askParseQ qsn parser = go
      where
        go = do
          putText qsn

          txt <- getStrippedLine
          case parser txt of
            EitherLeft err -> do
              putTextLn $ "Bad response: " <> pack err
              go
            EitherRight x -> pure x

    askParseEmptyQ :: Text -> (Text -> EitherString a) -> m (Maybe a)
    askParseEmptyQ qsn parser = go
      where
        go = do
          putText qsn

          getStrippedLineEmpty >>= \case
            Nothing -> pure Nothing
            Just txt ->
              case parser txt of
                EitherLeft err -> do
                  putTextLn $ "Bad answer: " <> pack err
                  go
                EitherRight x -> pure $ Just x

-- | Delete a task.
deleteTask ::
  ( HasCallStack,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadTerminal m,
    MonadTime m,
    MonadThrow m
  ) =>
  -- | Path to tasks.json.
  OsPath ->
  -- | Task id to delete.
  TaskId ->
  m ()
deleteTask tasksPath taskId = do
  index <- Index.readIndex tasksPath
  case Index.delete taskId index of
    Left err -> throwM err
    Right (newIndex, st) -> do
      rendered <- Render.renderOneNoStyle st

      noBuffering

      putTextLn "Found task:\n"
      putTextLn $ builderToTxt rendered

      ans <- askYesNoQ "Are you sure you want to delete (y/n)? "

      when ans $ do
        Index.writeIndex tasksPath newIndex
        putTextLn "Successfully deleted task"

      lineBuffering

-- | Lists tasks from the given file.
listTasks ::
  ( HasCallStack,
    MonadFileReader m,
    MonadTerminal m,
    MonadThrow m,
    MonadTime m
  ) =>
  -- | Path to tasks.json.
  OsPath ->
  -- | Is color enabled.
  ColorSwitch ->
  -- | Is unicode enabled.
  UnicodeSwitch ->
  -- | The sort type.
  Maybe SortType ->
  m ()
listTasks path color unicode msortType = do
  index <- Index.readIndex path

  let xs = Index.toList index
      sorted = Sorted.sortTasks msortType xs

  currTime <- getSystemZonedTime

  putTextLn
    $ TL.toStrict
    $ TLB.toLazyText
    $ Render.renderSorted currTime color unicode sorted

noBuffering :: (HasCallStack, MonadHandleWriter m) => m ()
noBuffering = setBuffering NoBuffering

lineBuffering :: (HasCallStack, MonadHandleWriter m) => m ()
lineBuffering = setBuffering LineBuffering

setBuffering :: (HasCallStack, MonadHandleWriter m) => BufferMode -> m ()
setBuffering buffMode = setBuff IO.stdin *> setBuff IO.stdout
  where
    setBuff h = HW.hSetBuffering h buffMode

askYesNoQ :: (HasCallStack, MonadTerminal m) => Text -> m Bool
askYesNoQ qsn = go
  where
    go = do
      putText qsn
      ans <- getStrippedLine

      if
        | ans == "y" -> pure True
        | ans == "n" -> pure False
        | otherwise -> do
            putTextLn "Bad answer, expected 'y' or 'n'."
            go

getStrippedLine :: (HasCallStack, MonadTerminal m) => m Text
getStrippedLine = T.strip . pack <$> getLine

getStrippedLineEmpty :: (HasCallStack, MonadTerminal m) => m (Maybe Text)
getStrippedLineEmpty =
  getStrippedLine <&> \txt ->
    if T.null txt
      then Nothing
      else Just txt
