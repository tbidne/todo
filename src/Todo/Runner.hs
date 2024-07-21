module Todo.Runner
  ( -- * Primary
    runTodo,

    -- * Misc
    withConfig,
    getConfig,
  )
where

import Todo qualified
import Todo.Configuration.Args (Args (noToml, tomlPath), getArgs)
import Todo.Configuration.Data.Command
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
import Todo.Configuration.Merged (Merged (coreConfig))
import Todo.Configuration.Merged qualified as Merged
import Todo.Configuration.Toml qualified as Toml
import Todo.Prelude

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
    Todo.deleteTask mergedConfig.coreConfig intMode mTaskIds
  CmdInsert -> Todo.insertTask mergedConfig.coreConfig
  CmdList mSortType revSort ->
    Todo.listTasks mergedConfig.coreConfig mSortType revSort
  CmdSetDescription intMode taskId taskDesc ->
    Todo.setTaskDescription mergedConfig.coreConfig intMode taskId taskDesc
  CmdSetDeadline intMode taskId taskDeadline ->
    Todo.setTaskDeadline mergedConfig.coreConfig intMode taskId taskDeadline
  CmdSetId intMode taskId newTaskId ->
    Todo.setTaskId mergedConfig.coreConfig intMode taskId newTaskId
  CmdSetPriority intMode taskId taskPriority ->
    Todo.setTaskPriority mergedConfig.coreConfig intMode taskId taskPriority
  CmdSetStatus intMode taskId taskStatus ->
    Todo.setTaskStatus mergedConfig.coreConfig intMode taskId taskStatus
