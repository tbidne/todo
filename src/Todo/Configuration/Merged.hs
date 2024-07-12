{-# LANGUAGE QuasiQuotes #-}

module Todo.Configuration.Merged
  ( Merged (..),
    mergeConfig,

    -- * Exceptions
    TaskNameLookupE (..),
    XdgIndexNotFoundE (..),
  )
where

import Data.Map.Strict qualified as Map
import Effects.FileSystem.PathReader (doesFileExist)
import Effects.FileSystem.Utils qualified as FsUtils
import System.OsPath qualified as FP
import Todo.Configuration.Args (Args (command, coreConfig), Command)
import Todo.Configuration.ConfigPhase
  ( ConfigPhase (ConfigPhaseMerged),
  )
import Todo.Configuration.Core
  ( CoreConfig (MkCoreConfig, colorSwitch, index, unicodeSwitch),
    IndexConfig (name, path),
  )
import Todo.Configuration.Default (fromDefault, (<.>))
import Todo.Configuration.Toml (Toml (coreConfig, taskNamePathMap))
import Todo.Index qualified as Index
import Todo.Prelude

data Merged = MkMerged
  { coreConfig :: CoreConfig ConfigPhaseMerged,
    command :: Command
  }
  deriving stock (Eq, Show)

mergeConfig ::
  ( HasCallStack,
    MonadFileReader m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  Args ->
  Maybe (Tuple2 OsPath Toml) ->
  m Merged
mergeConfig args Nothing = do
  let mIndexPath = args.coreConfig.index.path
      tasksPathArgs = case mIndexPath of
        Just indexPath -> TasksPathArgsPath indexPath
        Nothing -> TasksPathArgsXdg

  indexPath <- getTasksPath tasksPathArgs
  index <- Index.readIndex indexPath

  pure
    $ MkMerged
      { coreConfig =
          MkCoreConfig
            { colorSwitch =
                fromDefault args.coreConfig.colorSwitch,
              index,
              unicodeSwitch =
                fromDefault args.coreConfig.unicodeSwitch
            },
        command = args.command
      }
mergeConfig args (Just (tomlPath, toml)) = do
  let mIndexName =
        args.coreConfig.index.name <|> toml.coreConfig.index.name
      mIndexPath =
        args.coreConfig.index.path <|> toml.coreConfig.index.path

      tasksPathArgs = case mIndexPath of
        Just indexPath -> TasksPathArgsPath indexPath
        Nothing -> case mIndexName of
          Just indexName ->
            TasksPathArgsMap indexName tomlPath toml.taskNamePathMap
          Nothing -> TasksPathArgsXdg

  indexPath <- getTasksPath tasksPathArgs
  index <- Index.readIndex indexPath

  pure
    $ MkMerged
      { coreConfig =
          MkCoreConfig
            { colorSwitch =
                args.coreConfig.colorSwitch <.> toml.coreConfig.colorSwitch,
              index,
              unicodeSwitch =
                args.coreConfig.unicodeSwitch <.> toml.coreConfig.unicodeSwitch
            },
        command = args.command
      }

-- | Args for finding the index path.
data TasksPathArgs
  = -- | Explicit index path given, use it.
    TasksPathArgsPath OsPath
  | -- | No explicit path given, but we do have index name. Look it up in the
    -- map (the other OsPath is the toml file's path, used for looking up
    -- relative paths).
    TasksPathArgsMap Text OsPath (Map Text OsPath)
  | -- | No path or index name given. Lookup Xdg.
    TasksPathArgsXdg

-- | Retrieves the path to the tasks json file based on the configuration.
-- The semantics are:
--
--   1. If a definite path @p@ is given, use it.
--   2. Otherwise, if we are given a task name @n@, return the result of
--      looking up @n@ in @map@. If @n@ does not exist in @map@, throw an
--      error.
--   3. Fall back to XDG config e.g. ~/.config/todo/index.json. If this does
--      not exists, throw an error.
getTasksPath ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m
  ) =>
  TasksPathArgs ->
  m OsPath
getTasksPath (TasksPathArgsPath p) = pure p
getTasksPath (TasksPathArgsMap taskName tomlPath taskNamePathMap) =
  -- We support absolute path and paths relative to the toml file itself.
  -- No other relative paths are allowed (e.g. relative to current directory
  -- is not handled).
  case Map.lookup taskName taskNamePathMap of
    Just path ->
      if FP.isAbsolute path
        then pure path
        else
          let dir = FP.takeDirectory tomlPath
           in pure $ dir </> path
    Nothing -> throwM $ MkTaskNameLookupE taskName
getTasksPath TasksPathArgsXdg = do
  xdgConfigDir <- getTodoXdgConfig
  let tasksPath = xdgConfigDir </> [osp|index.json|]
  exists <- doesFileExist tasksPath
  if exists
    then pure tasksPath
    else throwM $ MkXdgIndexNotFoundE tasksPath

newtype TaskNameLookupE = MkTaskNameLookupE Text
  deriving stock (Eq, Show)

instance Exception TaskNameLookupE where
  displayException (MkTaskNameLookupE name) =
    mconcat
      [ "No task with name '",
        unpack name,
        "' found in task map."
      ]

newtype XdgIndexNotFoundE = MkXdgIndexNotFoundE OsPath
  deriving stock (Eq, Show)

instance Exception XdgIndexNotFoundE where
  displayException (MkXdgIndexNotFoundE p) =
    mconcat
      [ "No index name or path was given, so we fell back to XDG config '",
        FsUtils.decodeOsToFpLenient p,
        "', but none were found."
      ]
