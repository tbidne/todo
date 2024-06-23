{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Functional.Prelude
  ( module X,

    -- * Test Env
    TestEnv (..),

    -- * Runners
    run,
    runResponsesTodo,
    runException,

    -- * Misc
    getTestDir,
    toBSL,
    cfp,
  )
where

import Control.Monad.Reader
  ( MonadReader,
    ReaderT (runReaderT),
    asks,
  )
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Proxy as X (Proxy (Proxy))
import Effects.Exception (throwString, tryCS)
import Effects.FileSystem.HandleWriter (MonadHandleWriter)
import Effects.FileSystem.PathWriter as X (MonadPathWriter, copyFileWithMetadata)
import Effects.FileSystem.PathWriter qualified as PW
import Effects.FileSystem.Utils as X (unsafeDecodeOsToFp)
import Effects.FileSystem.Utils qualified as FsUtils
import Effects.System.Environment (MonadEnv (withArgs))
import Effects.System.Terminal (MonadTerminal (putStr))
import Test.Tasty as X (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Golden as X (goldenVsString)
import Todo.Prelude as X
import Todo.Runner qualified as Runner

-- | Test environment.
newtype TestEnv = MkTestEnv
  { testDir :: OsPath
  }
  deriving stock (Eq, Show)

-- | Environment for 'FuncIO'.
data FuncEnv = MkFuncEnv
  { terminalRef :: IORef Text,
    terminalResponsesRef :: IORef (List Text)
  }

-- | Runs the todo application.
newtype FuncIO a = MkFuncIO (ReaderT FuncEnv IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadEnv,
      MonadFail,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleWriter,
      MonadIO,
      MonadOptparse,
      MonadPathReader,
      MonadReader FuncEnv,
      MonadTime,
      MonadThrow
    )
    via (ReaderT FuncEnv IO)

instance MonadTerminal FuncIO where
  putStr s = do
    terminalRef <- asks (.terminalRef)
    liftIO $ modifyIORef' terminalRef (\xs -> xs <> pack s)

  getLine = do
    responsesRef <- asks (.terminalResponsesRef)
    responses <- liftIO . readIORef $ responsesRef
    case responses of
      [] -> error "getLine: Expected response but received empty"
      (r : rs) -> do
        liftIO $ writeIORef responsesRef rs
        pure $ unpack r

runFuncIO :: FuncEnv -> FuncIO a -> IO a
runFuncIO funcEnv (MkFuncIO rdr) = runReaderT rdr funcEnv

-- TODO: Rename to runTodo
run ::
  List String ->
  IO Text
run args = do
  terminalResponsesRef <- newIORef []
  terminalRef <- newIORef ""

  let funcEnv = MkFuncEnv terminalRef terminalResponsesRef

  withArgs args (runFuncIO funcEnv Runner.runTodo)

  readIORef terminalRef

runResponsesTodo ::
  List Text ->
  List String ->
  IO Text
runResponsesTodo responses args = do
  terminalResponsesRef <- newIORef responses
  terminalRef <- newIORef ""

  let funcEnv = MkFuncEnv terminalRef terminalResponsesRef

  withArgs args (runFuncIO funcEnv Runner.runTodo)

  readIORef terminalRef

-- TODO: Rename to runTodoException
runException ::
  forall e.
  (Exception e) =>
  List String ->
  IO Text
runException args = do
  terminalResponsesRef <- newIORef []
  terminalRef <- newIORef ""

  let funcEnv = MkFuncEnv terminalRef terminalResponsesRef

  eResult <- tryCS @_ @e $ withArgs args (runFuncIO funcEnv Runner.runTodo)

  case eResult of
    Right _ -> throwString "Expected exception, received success"
    Left ex -> pure (pack $ displayException ex)

toBSL :: Text -> BSL.ByteString
toBSL = (<> "\n") . BSL.fromStrict . encodeUtf8

cfp :: FilePath -> FilePath -> FilePath
cfp = FsUtils.combineFilePaths

-- | Appends the given directory onto the test directory, creating the
-- new directory if necessary.
getTestDir :: (MonadIO m, MonadPathWriter m) => IO TestEnv -> OsPath -> m OsPath
getTestDir testEnv path = do
  testDir <- liftIO $ (.testDir) <$> testEnv
  let path' = testDir </> path
  PW.createDirectoryIfMissing True path'
  pure path'
