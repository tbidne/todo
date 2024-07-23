module Todo.Cli
  ( -- * Primary
    runTodo,

    -- * Misc
    withConfig,
    getConfig,
  )
where

import Todo.Cli.Command.Delete qualified as Delete
import Todo.Cli.Command.Insert qualified as Insert
import Todo.Cli.Command.List qualified as List
import Todo.Cli.Command.Update qualified as Update
import Todo.Cli.Configuration.Args (Args (noToml, tomlPath), getArgs)
import Todo.Cli.Configuration.Data.Command
  ( Command
      ( CmdDelete,
        CmdInsert,
        CmdList,
        CmdSetDeadline,
        CmdSetDescription,
        CmdSetId,
        CmdSetPriority,
        CmdSetStatus
      ),
  )
import Todo.Cli.Configuration.Merged (Merged (coreConfig))
import Todo.Cli.Configuration.Merged qualified as Merged
import Todo.Cli.Configuration.Toml qualified as Toml
import Todo.Cli.Prelude

-- | Runs todo app.
runTodo ::
  ( HasCallStack,
    MonadCatch m,
    MonadFail m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadOptparse m,
    MonadPathReader m,
    MonadTerminal m,
    MonadTime m
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

  mToml <-
    if args.noToml
      then pure Nothing
      else Toml.maybeReadToml args.tomlPath

  Merged.mergeConfig args mToml

withConfig ::
  ( HasCallStack,
    MonadCatch m,
    MonadFail m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadTerminal m,
    MonadTime m
  ) =>
  Merged ->
  m ()
withConfig mergedConfig = case mergedConfig.command of
  CmdDelete intMode mTaskIds ->
    Delete.deleteTask mergedConfig.coreConfig intMode mTaskIds
  CmdInsert -> Insert.insertTask mergedConfig.coreConfig
  CmdList mSortType revSort ->
    List.listTasks mergedConfig.coreConfig mSortType revSort
  CmdSetDescription intMode taskId taskDesc ->
    Update.setTaskDescription mergedConfig.coreConfig intMode taskId taskDesc
  CmdSetDeadline intMode taskId taskDeadline ->
    Update.setTaskDeadline mergedConfig.coreConfig intMode taskId taskDeadline
  CmdSetId intMode taskId newTaskId ->
    Update.setTaskId mergedConfig.coreConfig intMode taskId newTaskId
  CmdSetPriority intMode taskId taskPriority ->
    Update.setTaskPriority mergedConfig.coreConfig intMode taskId taskPriority
  CmdSetStatus intMode taskId taskStatus ->
    Update.setTaskStatus mergedConfig.coreConfig intMode taskId taskStatus
