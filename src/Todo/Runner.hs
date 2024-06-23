{-# LANGUAGE QuasiQuotes #-}

module Todo.Runner
  ( runTodo,
  )
where

import Effects.FileSystem.HandleWriter
  ( MonadHandleWriter,
  )
import Effects.FileSystem.PathReader (getXdgConfig)
import Todo qualified
import Todo.Data.Task.Render.Utils
  ( ColorSwitch (ColorOn),
    UnicodeSwitch (UnicodeOn),
  )
import Todo.Prelude
import Todo.Runner.Args
  ( Args (command, path),
    Command (CmdInsert, CmdList),
    getArgs,
  )

-- | Runs todo app.
runTodo ::
  ( HasCallStack,
    MonadFail m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadOptparse m,
    MonadPathReader m,
    MonadTerminal m,
    MonadTime m,
    MonadThrow m
  ) =>
  m ()
runTodo = do
  args <- getArgs
  tasksPath <- getPath args.path
  case args.command of
    CmdInsert -> Todo.insertTask tasksPath
    CmdList mColor mUnicode mSortType ->
      Todo.listTasks tasksPath (defColor mColor) (defUnicode mUnicode) mSortType
  where
    defColor = fromMaybe ColorOn
    defUnicode = fromMaybe UnicodeOn

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
