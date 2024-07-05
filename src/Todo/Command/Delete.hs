module Todo.Command.Delete
  ( deleteTask,
  )
where

import Todo.Command.Utils qualified as CUtils
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
    MonadHaskeline m,
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

      putTextLn "Found task:\n"
      putTextLn $ builderToTxt rendered

      ans <- CUtils.askYesNoQ "Are you sure you want to delete (y/n)? "

      when ans $ do
        Index.writeIndex tasksPath newIndex
        putTextLn "Successfully deleted task"
