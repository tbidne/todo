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
  ( SomeTask (SingleTask),
    Task
      ( MkTask,
        deadline,
        description,
        priority,
        status,
        taskId
      ),
  )
import Todo.Data.Task.Render qualified as Render
import Todo.Data.Task.Render.Utils (ColorSwitch, UnicodeSwitch)
import Todo.Data.Task.Sorted (SortType)
import Todo.Data.Task.Sorted qualified as Sorted
import Todo.Data.Task.TaskId qualified as TaskId
import Todo.Data.Task.TaskPriority qualified as TaskPriority
import Todo.Data.Task.TaskStatus qualified as TaskStatus
import Todo.Data.Timestamp qualified as Timestamp
import Todo.Index (DuplicateIdE (MkDuplicateIdE), (∈))
import Todo.Index qualified as Index
import Todo.Prelude

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
  noBuffering

  putText "Task id: "
  idTxt <- getStrippedLine

  taskId <- TaskId.parseTaskId idTxt
  index <- Index.readIndex tasksPath
  when (taskId ∈ index) $ throwM $ MkDuplicateIdE taskId

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

  lineBuffering

  let task =
        MkTask
          { deadline,
            description,
            priority,
            status,
            taskId
          }

  let idx' = Index.reallyUnsafeInsert (SingleTask task) index

  Index.writeIndex tasksPath idx'
  where
    getStrippedLine = T.strip . pack <$> getLine

    getStrippedLineEmpty =
      getStrippedLine <&> \txt ->
        if T.null txt
          then Nothing
          else Just txt

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
