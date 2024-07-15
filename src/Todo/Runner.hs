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
    Command (CmdDelete, CmdInsert, CmdList),
    getArgs,
  )
import Todo.Configuration.Core
  ( CoreConfig (colorSwitch, index, unicodeSwitch),
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
  CmdDelete taskIds -> Todo.deleteTask index colorSwitch unicodeSwitch taskIds
  CmdInsert -> Todo.insertTask index colorSwitch unicodeSwitch
  CmdList mSortType ->
    Todo.listTasks index colorSwitch unicodeSwitch mSortType
  where
    index = mergedConfig.coreConfig.index
    colorSwitch = mergedConfig.coreConfig.colorSwitch
    unicodeSwitch = mergedConfig.coreConfig.unicodeSwitch
