{-# LANGUAGE QuasiQuotes #-}

module Todo.Runner
  ( runTodo,
  )
where

import Effects.FileSystem.FileWriter qualified as FW
import Effects.FileSystem.HandleWriter
  ( MonadHandleWriter,
  )
import Effects.FileSystem.PathReader (getXdgConfig)
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter qualified as PW
import Effects.FileSystem.Utils qualified as FsUtils
import System.OsPath qualified as FP
import Todo qualified
import Todo.Prelude
import Todo.Render.Utils
  ( ColorSwitch (ColorOn),
    UnicodeSwitch (UnicodeOn),
  )
import Todo.Runner.Args
  ( Args (colorSwitch, command, path, unicodeSwitch),
    Command (CmdDelete, CmdInsert, CmdList),
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
    MonadPathWriter m,
    MonadTerminal m,
    MonadTime m,
    MonadThrow m
  ) =>
  m ()
runTodo = do
  args <- getArgs
  tasksPath <- getPath args.path

  let colorSwitch = fromMaybe ColorOn args.colorSwitch
      unicodeSwitch = fromMaybe UnicodeOn args.unicodeSwitch

  case args.command of
    CmdDelete taskId -> Todo.deleteTask tasksPath colorSwitch unicodeSwitch taskId
    CmdInsert -> Todo.insertTask tasksPath colorSwitch unicodeSwitch
    CmdList mSortType ->
      Todo.listTasks tasksPath colorSwitch unicodeSwitch mSortType

getPath ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  Maybe OsPath ->
  m OsPath
getPath (Just path) = createJsonIfNotExists path
getPath Nothing = do
  xdgConfigDir <- getXdgConfig [osp|todo|]
  createJsonIfNotExists $ xdgConfigDir </> [osp|tasks.json|]

createJsonIfNotExists ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  OsPath ->
  m OsPath
createJsonIfNotExists path = do
  exists <- PR.doesFileExist path
  if exists
    then pure path
    else do
      putTextLn
        $ mconcat
          [ "File does not exist at path: '",
            pack (FsUtils.decodeOsToFpShow path),
            "'. Creating one."
          ]
      let dirName = FP.takeDirectory path
      PW.createDirectoryIfMissing True dirName

      FW.writeFileUtf8 path "[]"
      pure path
