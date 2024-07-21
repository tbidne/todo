{-# LANGUAGE QuasiQuotes #-}

{- HLINT ignore "Replace case with maybe" -}

module Todo.Configuration.Merged
  ( Merged (..),
    mergeConfig,
  )
where

import Data.Map.Strict qualified as Map
import Effects.FileSystem.PathReader (doesFileExist)
import System.OsPath qualified as FP
import Todo.Configuration.Args (Args (command, coreConfig))
import Todo.Configuration.ConfigPhase
  ( ConfigPhase (ConfigPhaseMerged),
  )
import Todo.Configuration.Core
  ( CoreConfig (MkCoreConfig, colorSwitch, index, unicodeSwitch),
    IndexConfig (name, path),
  )
import Todo.Configuration.Data.Command
  ( Command (CmdList),
    CommandArgs,
    CommandMerged,
  )
import Todo.Configuration.Data.Command qualified as Command
import Todo.Configuration.Default (fromDefault, (<.>))
import Todo.Configuration.Toml (Toml (coreConfig, taskNamePathMap))
import Todo.Exception
  ( IndexNameLookupE (MkIndexNameLookupE),
    XdgIndexNotFoundE (MkXdgIndexNotFoundE),
  )
import Todo.Index qualified as Index
import Todo.Prelude

data Merged = MkMerged
  { coreConfig :: CoreConfig ConfigPhaseMerged,
    command :: CommandMerged
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
        Just indexPath -> IndexPathArgsPath indexPath
        Nothing -> IndexPathArgsXdg

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
        command = Command.advancePhase args.command
      }
mergeConfig args (Just (tomlPath, toml)) = do
  let mIndexName =
        args.coreConfig.index.name <|> toml.coreConfig.index.name
      mIndexPath = args.coreConfig.index.path

      indexPathArgs = case mIndexPath of
        Just indexPath -> IndexPathArgsPath indexPath
        Nothing -> case mIndexName of
          Just indexName ->
            IndexPathArgsMap indexName tomlPath toml.taskNamePathMap
          Nothing -> IndexPathArgsXdg

  indexPath <- getTasksPath indexPathArgs
  index <- Index.readIndex indexPath

  let command = updateCommand args.command toml

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
        command
      }

updateCommand :: CommandArgs -> Toml -> CommandMerged
updateCommand command toml = case command of
  CmdList mSortType mRevSort ->
    CmdList
      (mSortType <|> preview (#listToml %? #sortType % _Just) toml)
      (mRevSort <.> preview (#listToml %? #reverse % _Just) toml)
  other -> Command.advancePhase other

-- | Args for finding the index path.
data IndexPathArgs
  = -- | Explicit index path given, use it.
    IndexPathArgsPath OsPath
  | -- | No explicit path given, but we do have index name. Look it up in the
    -- map (the other OsPath is the toml file's path, used for looking up
    -- relative paths).
    IndexPathArgsMap Text OsPath (Map Text OsPath)
  | -- | No path or index name given. Lookup Xdg.
    IndexPathArgsXdg

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
  IndexPathArgs ->
  m OsPath
getTasksPath (IndexPathArgsPath p) = pure p
getTasksPath (IndexPathArgsMap taskName tomlPath taskNamePathMap) =
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
    Nothing -> throwM $ MkIndexNameLookupE tomlPath taskName
getTasksPath IndexPathArgsXdg = do
  xdgConfigDir <- getTodoXdgConfig
  let tasksPath = xdgConfigDir </> [osp|index.json|]
  exists <- doesFileExist tasksPath
  if exists
    then pure tasksPath
    else throwM $ MkXdgIndexNotFoundE tasksPath
