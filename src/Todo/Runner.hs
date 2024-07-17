module Todo.Runner
  ( -- * Primary
    runTodo,

    -- * Misc
    withConfig,
    getConfig,
  )
where

import Todo qualified
import Todo.Configuration.Args
  ( Args (tomlPath),
    Command
      ( CmdDelete,
        CmdInsert,
        CmdList,
        CmdSetDeadline,
        CmdSetDescription,
        CmdSetId,
        CmdSetPriority,
        CmdSetStatus
      ),
    getArgs,
  )
import Todo.Configuration.Merged (Merged (coreConfig))
import Todo.Configuration.Merged qualified as Merged
import Todo.Configuration.Toml qualified as Toml
import Todo.Prelude

-- | Runs todo app.
runTodo ::
  ( HasCallStack,
    MonadFail m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadOptparse m,
    MonadPathReader m,
    MonadTerminal m,
    MonadTime m,
    MonadThrow m
  ) =>
  m ()
runTodo = getConfig >>= withConfig

getConfig ::
  ( HasCallStack,
    MonadFileReader m,
    MonadOptparse m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  m Merged
getConfig = do
  args <- getArgs
  mToml <- Toml.maybeReadToml args.tomlPath

  Merged.mergeConfig args mToml

withConfig ::
  ( HasCallStack,
    MonadFail m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadTerminal m,
    MonadTime m,
    MonadThrow m
  ) =>
  Merged ->
  m ()
withConfig mergedConfig = case mergedConfig.command of
  CmdDelete taskIds -> Todo.deleteTask mergedConfig.coreConfig taskIds
  CmdInsert -> Todo.insertTask mergedConfig.coreConfig
  CmdList mSortType ->
    Todo.listTasks mergedConfig.coreConfig mSortType
  CmdSetDescription taskId taskDesc ->
    Todo.setTaskDescription mergedConfig.coreConfig taskId taskDesc
  CmdSetDeadline taskId taskDeadline ->
    Todo.setTaskDeadline mergedConfig.coreConfig taskId taskDeadline
  CmdSetId taskId newTaskId ->
    Todo.setTaskId mergedConfig.coreConfig taskId newTaskId
  CmdSetPriority taskId taskPriority ->
    Todo.setTaskPriority mergedConfig.coreConfig taskId taskPriority
  CmdSetStatus taskId taskStatus ->
    Todo.setTaskStatus mergedConfig.coreConfig taskId taskStatus
