{-# LANGUAGE QuasiQuotes #-}

module Todo.Configuration.Toml
  ( -- * Primary
    Toml (..),
    maybeReadToml,

    -- * Exceptions
    ConfigNotFoundE (..),
  )
where

import Data.Map.Strict qualified as Map
import Effects.FileSystem.FileReader qualified as FR
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.Utils qualified as FsUtils
import TOML (DecodeTOML (tomlDecoder), Decoder, getFieldOptWith, getFieldWith)
import TOML qualified
import Todo.Configuration.ConfigPhase
  ( ConfigPhase (ConfigPhaseToml),
  )
import Todo.Configuration.Core
  ( CoreConfig (MkCoreConfig, colorSwitch, index, unicodeSwitch),
    IndexConfig (MkIndexConfig, name, path),
  )
import Todo.Prelude
import Todo.Render.Utils (ColorSwitch, UnicodeSwitch)

-- TODO:
-- - sort type
-- - timestamp color thresholw
-- - timezones, perhaps

data Toml = MkToml
  { coreConfig :: CoreConfig ConfigPhaseToml,
    taskNamePathMap :: Map Text OsPath
  }
  deriving stock (Eq, Show)

instance DecodeTOML Toml where
  tomlDecoder = do
    colorSwitch <- decodeColorSwitch
    indexName <- decodeIndexName
    indexPath <- decodeIndexPath
    unicodeSwitch <- decodeUnicodeSwitch

    taskNamePathMap <- decodeTaskNamePathMap

    pure
      $ MkToml
        { coreConfig =
            MkCoreConfig
              { colorSwitch,
                index =
                  MkIndexConfig
                    { name = indexName,
                      path = indexPath
                    },
                unicodeSwitch
              },
          taskNamePathMap
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

decodeIndexPath :: Decoder (Maybe OsPath)
decodeIndexPath = getFieldOptWith decoder "index-path"
  where
    decoder :: Decoder OsPath
    decoder = tomlDecoder >>= FsUtils.encodeFpToValidOsFail

decodeUnicodeSwitch :: Decoder (Maybe UnicodeSwitch)
decodeUnicodeSwitch = getFieldOptWith tomlDecoder "unicode"

maybeReadToml ::
  ( HasCallStack,
    MonadFileReader m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  Maybe OsPath ->
  m (Maybe Toml)
maybeReadToml Nothing = do
  tomlPath <- (</> [osp|config.toml|]) <$> getTodoXdgConfig
  exists <- PR.doesFileExist tomlPath
  if exists
    then Just <$> decodeToml tomlPath
    else pure Nothing
maybeReadToml (Just tomlPath) = do
  exists <- PR.doesFileExist tomlPath
  unless exists $ throwM $ MkConfigNotFoundE tomlPath

  Just <$> decodeToml tomlPath

decodeToml ::
  ( HasCallStack,
    MonadFileReader m,
    MonadThrow m
  ) =>
  OsPath ->
  m Toml
decodeToml path = do
  contents <- FR.readFileUtf8ThrowM path
  throwLeft $ TOML.decode contents

newtype ConfigNotFoundE = MkConfigNotFoundE OsPath
  deriving stock (Eq, Show)

instance Exception ConfigNotFoundE where
  displayException (MkConfigNotFoundE p) =
    mconcat
      [ "Config file not found: '",
        FsUtils.decodeOsToFpLenient p,
        "'"
      ]
