module Todo
  ( -- * Info
    listTasks,

    -- * Update
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
    TaskGroup (..),
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
    MonadThrow m
  ) =>
  OsPath ->
  m ()
insertTask tasksPath = do
  index <- Index.readIndex tasksPath

  noBuffering

  shouldMkTaskGroup <- askYesNoQ "Create task group (y/n)? "

  index' <-
    if shouldMkTaskGroup
      then do
        taskGroup <- mkTaskGroup index
        pure $ Index.reallyUnsafeInsert (MultiTask taskGroup) index
      else do
        task <- mkOneTask index
        pure $ Index.reallyUnsafeInsert (SingleTask task) index

  lineBuffering

  Index.writeIndex tasksPath index'
  where
    mkTaskGroup :: Index -> m TaskGroup
    mkTaskGroup index = do
      putText "Task group id: "
      taskId <- getTaskId index

      putText "Task status (leave blank for none): "
      status <- traverse TaskStatus.parseTaskStatus =<< getStrippedLineEmpty

      putText "Task priority (leave blank for none): "
      priority <- traverse TaskPriority.parseTaskPriority =<< getStrippedLineEmpty

      putTextLn "Now enter subtask(s) information"

      t <- mkOneTask index

      subtasks' <- whileM getMoreTasksAns (SingleTask <$> mkOneTask index)

      pure
        $ MkTaskGroup
          { taskId,
            priority,
            status,
            subtasks = SingleTask t :<|| subtasks'
          }

    mkOneTask :: Index -> m Task
    mkOneTask index = do
      putText "Task id: "
      taskId <- getTaskId index

      putText "Task status: "
      statusTxt <- getStrippedLine
      status <- TaskStatus.parseTaskStatus statusTxt

      putText "Task priority: "
      priorityTxt <- getStrippedLine
      priority <- TaskPriority.parseTaskPriority priorityTxt

      putText "Description (leave blank for none): "
      description <- getStrippedLineEmpty

      putText "Deadline (leave blank for none): "
      deadlineTxt <- getStrippedLineEmpty
      deadline <- traverse (Timestamp.parseTimestamp . unpack) deadlineTxt

      pure
        $ MkTask
          { deadline,
            description,
            priority,
            status,
            taskId
          }

    getTaskId :: Index -> m TaskId
    getTaskId index = do
      idTxt <- getStrippedLine
      taskId <- TaskId.parseTaskId idTxt
      when (taskId ∈ index) $ throwM $ MkDuplicateIdE taskId
      pure taskId

    getMoreTasksAns :: m Bool
    getMoreTasksAns = askYesNoQ "Another task (y/n)? "

    askYesNoQ :: Text -> m Bool
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

    getStrippedLine :: m Text
    getStrippedLine = T.strip . pack <$> getLine

    getStrippedLineEmpty :: m (Maybe Text)
    getStrippedLineEmpty =
      getStrippedLine <&> \txt ->
        if T.null txt
          then Nothing
          else Just txt

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
