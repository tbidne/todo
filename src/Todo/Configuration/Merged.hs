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
  Maybe Toml ->
  m Merged
mergeConfig args Nothing = do
  let mIndexPath = args.coreConfig.index.path

  indexPath <- getTasksPath Nothing Map.empty mIndexPath
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
mergeConfig args (Just toml) = do
  let mIndexName =
        args.coreConfig.index.name <|> toml.coreConfig.index.name
      mIndexPath =
        args.coreConfig.index.path <|> toml.coreConfig.index.path

  -- TODO: Right now this requires either an absolute path. We should at
  -- least allow:
  --
  --   - paths relative to config (e.g. index-legend entry { path: 'a_path'})
  --   - maybe relative to the current directory?
  indexPath <- getTasksPath mIndexName toml.taskNamePathMap mIndexPath
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
  (HasCallStack, MonadPathReader m, MonadThrow m) =>
  -- | Task name @n@.
  Maybe Text ->
  -- | Name -> Path map, @map@.
  Map Text OsPath ->
  -- | Task path @p@.
  Maybe OsPath ->
  m OsPath
getTasksPath _ _ (Just p) = pure p
getTasksPath (Just taskName) taskNamePathMap Nothing =
  case Map.lookup taskName taskNamePathMap of
    Just path -> pure path
    Nothing -> throwM $ MkTaskNameLookupE taskName
getTasksPath Nothing _ Nothing = do
  -- get XDG location
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
