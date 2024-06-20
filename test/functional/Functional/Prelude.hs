{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Functional.Prelude
  ( module X,
    run,
    runException,

    -- * Misc
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
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Proxy as X (Proxy (Proxy))
import Effects.Exception (throwString, tryCS)
import Effects.FileSystem.Utils qualified as FsUtils
import Effects.System.Environment (MonadEnv (withArgs))
import Effects.System.Terminal (MonadTerminal (putStr))
import Test.Tasty as X (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Golden as X (goldenVsString)
import Todo.Prelude as X
import Todo.Runner qualified as Runner

newtype FuncEnv = MkFuncEnv
  { terminalRef :: IORef Text
  }

newtype FuncIO a = MkFuncIO (ReaderT FuncEnv IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadEnv,
      MonadFileReader,
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
    liftIO $ modifyIORef' terminalRef (\xs -> pack s <> xs)

runFuncIO :: FuncEnv -> FuncIO a -> IO a
runFuncIO funcEnv (MkFuncIO rdr) = runReaderT rdr funcEnv

run ::
  List String ->
  IO Text
run args = do
  terminalRef <- newIORef ""

  let funcEnv = MkFuncEnv terminalRef

  withArgs args (runFuncIO funcEnv Runner.runTodo)

  readIORef terminalRef

runException ::
  forall e.
  (Exception e) =>
  List String ->
  IO Text
runException args = do
  terminalRef <- newIORef ""

  let funcEnv = MkFuncEnv terminalRef

  eResult <- tryCS @_ @e $ withArgs args (runFuncIO funcEnv Runner.runTodo)

  case eResult of
    Right _ -> throwString "Expected exception, received success"
    Left ex -> pure (pack $ displayException ex)

toBSL :: Text -> BSL.ByteString
toBSL = (<> "\n") . BSL.fromStrict . encodeUtf8

cfp :: FilePath -> FilePath -> FilePath
cfp = FsUtils.combineFilePaths
