{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Main (main) where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (bracket)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Word (Word32)
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter qualified as PR
import Effects.FileSystem.Utils qualified as FS
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    nfIO,
  )
import Todo.Cli.Command.List qualified as List
import Todo.Cli.Configuration.Core (CoreConfig (MkCoreConfig))
import Todo.Cli.Prelude hiding (IO)
import Todo.Cli.Render.Utils (ColorSwitch (ColorOff), UnicodeSwitch (UnicodeOff))
import Todo.Configuration.Default (Default (def))
import Todo.Data.Task
  ( SingleTask
      ( MkSingleTask,
        deadline,
        description,
        priority,
        status,
        taskId
      ),
    SomeTask (SomeTaskGroup, SomeTaskSingle),
    TaskGroup (MkTaskGroup, priority, status, subtasks, taskId),
  )
import Todo.Data.TaskId.Internal (TaskId (UnsafeTaskId))
import Todo.Data.TaskPriority (TaskPriority (Low))
import Todo.Data.TaskStatus (TaskStatus (NotStarted))
import Todo.Index qualified as Index
import Todo.Index.Internal (Index (UnsafeIndex))
import Prelude (IO)

main :: IO ()
main = bracket setup teardown runBenchmarks
  where
    runBenchmarks benchEnv =
      defaultMain
        [ bgroup
            "Shallow"
            [ runBench "shallow-100" benchEnv.shallow_100,
              runBench "shallow-1_000" benchEnv.shallow_1_000,
              runBench "shallow-10_000" benchEnv.shallow_10_000
            ],
          bgroup
            "Deep"
            [ runBench "deep-7" benchEnv.deep_7,
              runBench "deep-10" benchEnv.deep_10,
              runBench "deep-13" benchEnv.deep_13
            ]
        ]

newtype BenchIO a = MkBenchIO (IO a)
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadFileReader,
      MonadThrow,
      MonadTime
    )

instance MonadTerminal BenchIO where
  putStrLn = pure . rnf

runBenchIO :: BenchIO a -> IO a
runBenchIO (MkBenchIO m) = m

runBench :: String -> OsPath -> Benchmark
runBench desc path = bench desc $ nfIO $ runBenchIO $ do
  index <- Index.readIndex path
  let config = MkCoreConfig ColorOff index UnicodeOff
  List.listTasks config Nothing def

data BenchEnv = MkBenchEnv
  { benchDir :: OsPath,
    shallow_100 :: OsPath,
    shallow_1_000 :: OsPath,
    shallow_10_000 :: OsPath,
    deep_7 :: OsPath,
    deep_10 :: OsPath,
    deep_13 :: OsPath
  }
  deriving stock (Eq, Show)

setup :: IO BenchEnv
setup = do
  tmpDir <- PR.getTemporaryDirectory

  let benchDir =
        tmpDir
          </> [osp|todo|]
          </> [osp|bench|]

  PR.removeDirectoryRecursiveIfExists benchDir
  PR.createDirectoryIfMissing True benchDir

  shallow_100 <- createShallow benchDir [osp|shallow-100|] 100
  shallow_1_000 <- createShallow benchDir [osp|shallow-1_000|] 1_000
  shallow_10_000 <- createShallow benchDir [osp|shallow-10_000|] 10_000

  deep_7 <- createDeep benchDir [osp|deep-7|] 7
  deep_10 <- createDeep benchDir [osp|deep-10|] 10
  deep_13 <- createDeep benchDir [osp|deep-13|] 13

  pure
    $ MkBenchEnv
      { benchDir,
        shallow_100,
        shallow_1_000,
        shallow_10_000,
        deep_7,
        deep_10,
        deep_13
      }
  where
    createShallow :: OsPath -> OsPath -> Word32 -> IO OsPath
    createShallow benchDir name numTasks = do
      numOsPath <- FS.encodeFpToOsThrowM (show numTasks)
      let tasks = mkTask . showt <$> [1 .. numTasks]
          path = benchDir </> name <> [osp|_|] <> numOsPath
          idx = UnsafeIndex (listToSeq tasks) path

      Index.writeIndex idx
      pure path

    createDeep :: OsPath -> OsPath -> Word8 -> IO OsPath
    createDeep benchDir name depth = do
      numOsPath <- FS.encodeFpToOsThrowM (show depth)
      counterRef <- newIORef 0

      tasks <- go depth counterRef
      let path = benchDir </> name <> [osp|_|] <> numOsPath
          idx = UnsafeIndex (tasks :<| Empty) path

      Index.writeIndex idx
      pure path
      where
        go :: Word8 -> IORef Word32 -> IO SomeTask
        go 0 counterRef = do
          counter <- readIORef counterRef
          modifyIORef' counterRef (+ 1)
          pure $ mkTask (showt counter)
        go n counterRef = do
          counter <- readIORef counterRef
          modifyIORef' counterRef (+ 1)
          l <- go (n - 1) counterRef
          r <- go (n - 1) counterRef
          pure
            $ SomeTaskGroup
            $ MkTaskGroup
              { taskId = UnsafeTaskId $ showt counter,
                status = Nothing,
                priority = Nothing,
                subtasks = l :<| r :<| Empty
              }

    mkTask :: Text -> SomeTask
    mkTask i =
      SomeTaskSingle
        $ MkSingleTask
          { deadline = Nothing,
            description = Nothing,
            priority = Low,
            status = NotStarted,
            taskId = UnsafeTaskId i
          }

teardown :: BenchEnv -> IO ()
teardown = PR.removeDirectoryRecursive . (.benchDir)
