{-# LANGUAGE QuasiQuotes #-}

module Todo.Runner
  ( runTodo,
  )
where

import Effects.FileSystem.PathReader (getXdgConfig)
import Todo qualified
import Todo.Prelude
import Todo.Runner.Args (Args (command, path), Command (CmdList), getArgs)

-- | Runs todo app.
runTodo ::
  ( HasCallStack,
    MonadFileReader m,
    MonadOptparse m,
    MonadPathReader m,
    MonadTerminal m,
    MonadTime m,
    MonadThrow m
  ) =>
  m ()
runTodo = do
  args <- getArgs
  path <- getPath args.path
  case args.command of
    CmdList mColor mSortType -> Todo.listTasks path (defColor mColor) mSortType
  where
    defColor = fromMaybe True

getPath ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  Maybe OsPath ->
  m OsPath
getPath (Just path) = pure path
getPath Nothing = do
  xdgConfigDir <- getXdgConfig [osp|todo|]
  pure $ xdgConfigDir </> [osp|tasks.json|]
