{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Todo.Cli.Configuration.Toml
  ( -- * Primary
    Toml (..),
    maybeReadToml,
  )
where

import Data.Map.Strict qualified as Map
import Effects.FileSystem.FileReader qualified as FR
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.Utils qualified as FsUtils
import Optics.Core (review)
import TOML (DecodeTOML (tomlDecoder), Decoder, getFieldOptWith, getFieldWith)
import TOML qualified
import Todo.Cli.Configuration.ConfigPhase
  ( ConfigPhase (ConfigPhaseToml),
  )
import Todo.Cli.Configuration.Core
  ( CoreConfig (MkCoreConfig, colorSwitch, index, unicodeSwitch),
    IndexConfig (MkIndexConfig, name, path),
  )
import Todo.Cli.Prelude
import Todo.Cli.Render.Utils (ColorSwitch, UnicodeSwitch)
import Todo.Data.Sorted (SortType)
import Todo.Data.Sorted.RevSort (RevSort)
import Todo.Exception (ConfigNotFoundE (MkConfigNotFoundE))

-- TODO:
-- - timestamp color threshold
-- - timezones, perhaps

-- It is likely this will either go back in the main lib or in its own
-- todo-toml lib. But for now just move it to cli to make the split easier.

data ListToml = MkListToml
  { reverse :: Maybe RevSort,
    sortType :: Maybe SortType
  }
  deriving stock (Eq, Show)

instance
  (k ~ A_Lens, a ~ Maybe SortType, b ~ Maybe SortType) =>
  LabelOptic "sortType" k ListToml ListToml a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkListToml _reverse _sortType) ->
          fmap
            (MkListToml _reverse)
            (f _sortType)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe RevSort, b ~ Maybe RevSort) =>
  LabelOptic "reverse" k ListToml ListToml a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkListToml _reverse _sortType) ->
          fmap
            (`MkListToml` _sortType)
            (f _reverse)
  {-# INLINE labelOptic #-}

instance DecodeTOML ListToml where
  tomlDecoder = do
    reverse <- decodeReverse
    sortType <- decodeSortType
    pure
      $ MkListToml
        { reverse,
          sortType
        }
    where
      decodeReverse = getFieldOptWith (review #boolIso <$> tomlDecoder) "reverse"
      decodeSortType = getFieldOptWith tomlDecoder "sort-type"

data Toml = MkToml
  { coreConfig :: CoreConfig ConfigPhaseToml,
    listToml :: Maybe ListToml,
    taskNamePathMap :: Map Text OsPath
  }
  deriving stock (Eq, Show)

instance
  (k ~ A_Lens, a ~ Maybe ListToml, b ~ Maybe ListToml) =>
  LabelOptic "listToml" k Toml Toml a b
  where
  labelOptic =
    lensVL
      $ \f
         (MkToml _coreConfig _listToml _taskNamePathMap) ->
          fmap
            (\listToml' -> MkToml _coreConfig listToml' _taskNamePathMap)
            (f _listToml)
  {-# INLINE labelOptic #-}

instance DecodeTOML Toml where
  tomlDecoder = do
    colorSwitch <- decodeColorSwitch
    indexName <- decodeIndexName
    unicodeSwitch <- decodeUnicodeSwitch

    taskNamePathMap <- decodeTaskNamePathMap
    listToml <- getFieldOptWith tomlDecoder "list"

    pure
      $ MkToml
        { coreConfig =
            MkCoreConfig
              { colorSwitch,
                index =
                  MkIndexConfig
                    { name = indexName,
                      path = ()
                    },
                unicodeSwitch
              },
          taskNamePathMap,
          listToml
        }

decodeColorSwitch :: Decoder (Maybe ColorSwitch)
decodeColorSwitch = getFieldOptWith tomlDecoder "color"

data TaskNamePathPair = MkTaskNamePathPair Text String
  deriving stock (Eq, Show)

instance DecodeTOML TaskNamePathPair where
  tomlDecoder = do
    MkTaskNamePathPair
      <$> getFieldWith tomlDecoder "name"
      <*> getFieldWith tomlDecoder "path"

decodeTaskNamePathMap :: Decoder (Map Text OsPath)
decodeTaskNamePathMap =
  getFieldOptWith decoder "index-legend" <&> \case
    Nothing -> Map.empty
    Just mp -> mp
  where
    decoder =
      tomlDecoder >>= \nameFilePathList -> do
        nameOsPathList <- traverse encodePath nameFilePathList
        pure $ Map.fromList nameOsPathList

    encodePath :: TaskNamePathPair -> Decoder (Text, OsPath)
    encodePath (MkTaskNamePathPair n p) = (n,) <$> FsUtils.encodeFpToValidOsFail p

decodeIndexName :: Decoder (Maybe Text)
decodeIndexName = getFieldOptWith tomlDecoder "index-name"

decodeUnicodeSwitch :: Decoder (Maybe UnicodeSwitch)
decodeUnicodeSwitch = getFieldOptWith tomlDecoder "unicode"

maybeReadToml ::
  ( HasCallStack,
    MonadFileReader m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  Maybe OsPath ->
  m (Maybe (Tuple2 OsPath Toml))
maybeReadToml Nothing = do
  tomlPath <- (</> [osp|config.toml|]) <$> getTodoXdgConfig
  exists <- PR.doesFileExist tomlPath
  if exists
    then Just . (tomlPath,) <$> decodeToml tomlPath
    else pure Nothing
maybeReadToml (Just tomlPath) = do
  exists <- PR.doesFileExist tomlPath
  unless exists $ throwM $ MkConfigNotFoundE tomlPath

  Just . (tomlPath,) <$> decodeToml tomlPath

decodeToml ::
  ( HasCallStack,
    MonadFileReader m,
    MonadThrow m
  ) =>
  OsPath ->
  m Toml
decodeToml = FR.readFileUtf8ThrowM >=> throwLeft . TOML.decode
