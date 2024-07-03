{- HLINT ignore "Redundant bracket" -}

-- NOTE: HLINT ignore due to OverloadedRecordDot false positives e.g.
-- (some thing).field.

module Todo
  ( -- * Info
    listTasks,

    -- * Update
    deleteTask,
    Insert.insertTask,
  )
where

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Effects.FileSystem.HandleWriter
  ( MonadHandleWriter,
  )
import Effects.Time (MonadTime (getSystemZonedTime))
import Todo.Command.Insert qualified as Insert
import Todo.Command.Utils qualified as CUtils
import Todo.Data.Sorted (SortType)
import Todo.Data.Sorted qualified as Sorted
import Todo.Data.TaskId (TaskId)
import Todo.Index qualified as Index
import Todo.Prelude
import Todo.Render qualified as Render
import Todo.Render.Utils (ColorSwitch, UnicodeSwitch)

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
  -- | Is color enabled.
  ColorSwitch ->
  -- | Is unicode enabled.
  UnicodeSwitch ->
  -- | Task id to delete.
  TaskId ->
  m ()
deleteTask tasksPath color unicode taskId = do
  index <- Index.readIndex tasksPath
  case Index.delete taskId index of
    Left err -> throwM err
    Right (newIndex, st) -> do
      rendered <- Render.renderOne color unicode st

      CUtils.withNoBuffering $ do
        putTextLn "Found task:\n"
        putTextLn $ builderToTxt rendered

        ans <- CUtils.askYesNoQ "Are you sure you want to delete (y/n)? "

        when ans $ do
          Index.writeIndex tasksPath newIndex
          putTextLn "Successfully deleted task"

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
