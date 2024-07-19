{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-x-unimpl #-}

-- TODO: Remove -Wno-deprecations once on GHC 9.10+.

module Integration.Prelude
  ( module X,

    -- * Running tests
    runGetConfig,
    runGetConfigException,
    runXdgGetConfigException,

    -- * OS paths
    exampleConfigFilePath,
    exampleConfigOsPath,
    noPathConfigFilePath,
    noPathConfigOsPath,
    commandsConfigFilePath,
    commandsConfigOsPath,

    -- * Misc
    cfp,
    examplePath,
    testHedgehogOne,
    tomlFilePath,
    tomlOsPath,
    unsafeParseTimestamp,
  )
where

import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Effects.Exception (tryCS)
import Effects.FileSystem.PathReader
  ( MonadPathReader
      ( doesFileExist,
        getXdgDirectory
      ),
    XdgDirectory (XdgConfig),
  )
import Effects.FileSystem.Utils qualified as FsUtils
import Hedgehog as X
  ( PropertyName,
    PropertyT,
    annotate,
    assert,
    property,
    withTests,
    (===),
  )
import System.Environment (withArgs)
import Test.Tasty as X (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit as X
  ( Assertion,
    assertBool,
    assertFailure,
    testCase,
    (@=?),
  )
import Test.Tasty.Hedgehog as X (testPropertyNamed)
import Todo.Configuration.Merged (Merged)
import Todo.Data.Timestamp (Timestamp)
import Todo.Data.Timestamp qualified as Timestamp
import Todo.Prelude as X hiding (IO)
import Todo.Runner qualified as Runner
import Prelude as X (IO)

examplePath :: OsPath
examplePath = [osp|examples|] </> [osp|index.json|]

newtype IntEnv = MkIntEnv
  { xdgDir :: OsPath
  }
  deriving stock (Eq, Show)

defaultEnv :: IntEnv
defaultEnv = MkIntEnv defaultXdg

defaultXdg :: OsPath
defaultXdg = [osp|test|] </> [osp|integration|] </> [osp|toml|]

newtype IntIO a = MkIntIO (ReaderT IntEnv IO a)
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadFileReader,
      MonadIO,
      MonadOptparse,
      MonadReader IntEnv,
      MonadThrow
    )

instance MonadPathReader IntIO where
  getXdgDirectory XdgConfig d = do
    env <- ask
    pure $ env.xdgDir </> d
  getXdgDirectory _ _ = unimpl

  doesFileExist = liftIO . doesFileExist

runIntIO :: IntEnv -> IntIO a -> IO a
runIntIO env (MkIntIO rdr) = runReaderT rdr env

runGetConfig :: [String] -> IO Merged
runGetConfig args = withArgs args $ runIntIO defaultEnv Runner.getConfig

runGetConfigException :: (Exception e) => List String -> IO e
runGetConfigException args = withArgs args $ do
  eResult <- tryCS $ runIntIO defaultEnv Runner.getConfig
  case eResult of
    Left ex -> pure ex
    Right x -> throwString $ "Expected exception, received: " <> show x

runXdgGetConfigException :: (Exception e) => OsPath -> List String -> IO e
runXdgGetConfigException xdgDir args = withArgs args $ do
  eResult <- tryCS $ runIntIO env Runner.getConfig
  case eResult of
    Left ex -> pure ex
    Right x -> throwString $ "Expected exception, received: " <> show x
  where
    env = MkIntEnv xdgDir

testHedgehogOne :: TestName -> PropertyName -> PropertyT IO () -> TestTree
testHedgehogOne desc fnName m =
  testPropertyNamed desc fnName
    $ withTests 1
    $ property m

tomlOsPath :: OsPath
tomlOsPath = [osp|test|] </> [osp|integration|] </> [osp|toml|]

tomlFilePath :: FilePath
tomlFilePath = FsUtils.unsafeDecodeOsToFp tomlOsPath

cfp :: FilePath -> FilePath -> FilePath
cfp = FsUtils.combineFilePaths

unsafeParseTimestamp :: (HasCallStack) => String -> Timestamp
unsafeParseTimestamp s = case Timestamp.parseTimestamp s of
  Nothing -> error $ "Failed parsing timestamps: '" <> s <> "'"
  Just t -> t

exampleConfigFilePath :: FilePath
exampleConfigFilePath = FsUtils.unsafeDecodeOsToFp exampleConfigOsPath

exampleConfigOsPath :: OsPath
exampleConfigOsPath = [osp|examples|] </> [osp|config.toml|]

noPathConfigFilePath :: FilePath
noPathConfigFilePath = FsUtils.unsafeDecodeOsToFp noPathConfigOsPath

noPathConfigOsPath :: OsPath
noPathConfigOsPath = getIntTomlOsPath [osp|no-path.toml|]

commandsConfigFilePath :: FilePath
commandsConfigFilePath = FsUtils.unsafeDecodeOsToFp commandsConfigOsPath

commandsConfigOsPath :: OsPath
commandsConfigOsPath = getIntTomlOsPath [osp|commands.toml|]

getIntTomlOsPath :: OsPath -> OsPath
getIntTomlOsPath p =
  [osp|test|]
    </> [osp|integration|]
    </> [osp|toml|]
    </> p
