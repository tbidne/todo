{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Test.Cli.Functional.Prelude
  ( module X,

    -- * Test Env
    TestEnv (..),

    -- * Runners
    runTodo,
    runTodoResponses,
    runTodoException,

    -- * Golden runners
    testGoldenRunnerParams,
    testGoldenRunnerParamsNoEnv,
    GoldenParams (..),

    -- * Misc
    exampleJsonOsPath,
    getTestDir,
    mkInputDir,
    mkOutputDir,
    toBSL,
    toBS,
    writeActualFile,
    cfp,
  )
where

import Control.Monad.Reader
  ( MonadReader,
    ReaderT (runReaderT),
    asks,
  )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Proxy as X (Proxy (Proxy))
import Effects.FileSystem.HandleWriter (MonadHandleWriter)
import Effects.FileSystem.PathWriter as X (copyFileWithMetadata)
import Effects.FileSystem.PathWriter qualified as PW
import Effects.Haskeline (MonadHaskeline (getInputLine))
import Effects.System.Environment (MonadEnv (withArgs))
import Effects.System.Terminal (MonadTerminal (putStr))
import FileSystem.OsPath as X (unsafeDecode, unsafeEncodeValid)
import FileSystem.OsPath qualified as OsPath
import Test.Cli.Functional.Prelude.GoldenParams (GoldenParams (..))
import Test.Tasty as X (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Golden as X (goldenVsFile)
import Todo.Cli qualified as Cli
import Todo.Cli.Prelude as X

-- | Test environment.
newtype TestEnv = MkTestEnv
  { testDir :: OsPath
  }
  deriving stock (Eq, Show)

-- | Environment for 'FuncIO'.
data FuncEnv = MkFuncEnv
  { terminalRef :: IORef (Seq Text),
    terminalResponsesRef :: IORef (List Text)
  }

-- | Runs the todo application.
newtype FuncIO a = MkFuncIO (ReaderT FuncEnv IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadEnv,
      MonadFail,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleWriter,
      MonadIO,
      MonadOptparse,
      MonadPathReader,
      MonadPathWriter,
      MonadReader FuncEnv,
      MonadTime,
      MonadThrow
    )
    via (ReaderT FuncEnv IO)

instance MonadTerminal FuncIO where
  putStr s = do
    terminalRef <- asks (.terminalRef)
    liftIO $ modifyIORef' terminalRef (:|> pack s)

instance MonadHaskeline FuncIO where
  getInputLine s = do
    -- no newline so we can print response after the prompt.
    putStr s
    responsesRef <- asks (.terminalResponsesRef)
    responses <- liftIO . readIORef $ responsesRef
    case responses of
      [] -> error "getInputLine: Expected response but received empty"
      (r : rs) -> do
        liftIO $ writeIORef responsesRef rs
        -- print response, simulating what the output looks like to an actual
        -- user
        putTextLn r
        pure $ Just $ unpack r

runFuncIO :: FuncEnv -> FuncIO a -> IO a
runFuncIO funcEnv (MkFuncIO rdr) = runReaderT rdr funcEnv

-- | Runs todo.
runTodo ::
  -- | CLI args
  List String ->
  IO Text
runTodo = runTodoResponses []

-- | Runs todo with terminal responses.
runTodoResponses ::
  -- | Terminal responses
  List Text ->
  -- | CLI args
  List String ->
  IO Text
runTodoResponses responses args = do
  terminalResponsesRef <- newIORef responses
  terminalRef <- newIORef Empty

  let funcEnv = MkFuncEnv terminalRef terminalResponsesRef

  withArgs args' (runFuncIO funcEnv Cli.runTodo)

  concatSeq <$> readIORef terminalRef
  where
    args' = "--no-config" : args

-- | Runs todo, expecting an exception
runTodoException ::
  forall e.
  (Exception e) =>
  -- | CLI args
  List String ->
  IO Text
runTodoException args = do
  eResult <- try @_ @e $ runTodo args

  case eResult of
    Right _ -> throwText "Expected exception, received success"
    Left ex -> pure (pack $ displayException ex)

-- | Runs a golden test with the params.
testGoldenRunnerParams :: GoldenParams -> IO TestEnv -> TestTree
testGoldenRunnerParams goldenParams testEnv =
  testGoldenRunnerParamsMTestEnv goldenParams (Just testEnv)

-- | Runs a golden test with the params and no test env.
testGoldenRunnerParamsNoEnv :: GoldenParams -> TestTree
testGoldenRunnerParamsNoEnv goldenParams =
  testGoldenRunnerParamsMTestEnv goldenParams Nothing

testGoldenRunnerParamsMTestEnv :: GoldenParams -> Maybe (IO TestEnv) -> TestTree
testGoldenRunnerParamsMTestEnv goldenParams mTestEnv =
  goldenVsFile goldenParams.testDesc goldenPath actualPath $ do
    let indexPath = fromMaybe exampleJsonOsPath goldenParams.indexPath

    testIndexPath <- case mTestEnv of
      -- 1. If we are given a TestEnv then it is possible we are modifying
      -- the original index in some way. Thus we need to copy it over
      -- to the tmp test dir and use that path instead.
      Just testEnv -> do
        testDir <- getTestDir testEnv testDirWithNameOsPath
        let newPath = testDir </> [osp|index.json|]

        -- copy example to test dir
        copyFileWithMetadata indexPath newPath

        pure newPath
      -- 2. If no TestEnv then we assume that we are not modifying the
      -- original index.
      Nothing -> pure indexPath

    let cmdArgs =
          [ "--index-path",
            unsafeDecode testIndexPath,
            "--color",
            "off"
          ]
            ++ goldenParams.args

    let runner = fromMaybe runTodo goldenParams.runner

    -- run main cmd
    cmdResult <- runner cmdArgs

    listResult <-
      if goldenParams.runList
        then do
          let listArgs =
                [ "--index-path",
                  unsafeDecode testIndexPath,
                  "--color",
                  "off",
                  "list"
                ]

          -- run list
          ("\n\n" <>) <$> runTodo listArgs
        else pure ""

    writeActualFile actualPath (cmdResult <> listResult)
  where
    testDirWithNameOsPath = goldenParams.dataDir </> goldenParams.testDirName

    outputPathStart =
      unsafeDecode
        $ [ospPathSep|lib/cli/test/functional/Test/Cli/Functional|]
        </> goldenParams.dataDir
        </> [osp|output|]
        </> goldenParams.testDirName

    actualPath = outputPathStart <> ".actual"
    goldenPath = outputPathStart <> ".golden"

toBSL :: Text -> BSL.ByteString
toBSL = BSL.fromStrict . toBS

toBS :: Text -> ByteString
toBS = (<> "\n") . encodeUtf8

writeActualFile :: (MonadFileWriter m) => FilePath -> Text -> m ()
writeActualFile actualPath result =
  writeBinaryFile (unsafeEncodeValid actualPath) (toBS result)

cfp :: FilePath -> FilePath -> FilePath
cfp = OsPath.combineFilePaths

-- | Appends the given directory onto the test directory, creating the
-- new directory if necessary.
getTestDir :: (MonadIO m, MonadPathWriter m) => IO TestEnv -> OsPath -> m OsPath
getTestDir testEnv path = do
  testDir <- liftIO $ (.testDir) <$> testEnv
  let path' = testDir </> path
  PW.createDirectoryIfMissing True path'
  pure path'

-- Do __not__ intersperse newlines here. The terminal is effectively doing
-- that automatically via putStrLn. If we add newlines here, then every line
-- is separated by an extra newline w.r.t. the actual output.
--
-- An alternative that is nearly identical is to implement putStrLn instead
-- of putStr. This way we do not append a newline after every call
-- (like the default impl of putStr), and then add the newlines here.
concatSeq :: Seq Text -> Text
concatSeq = fold

exampleJsonOsPath :: OsPath
exampleJsonOsPath = [ospPathSep|examples/index.json|]

-- | Takes a name like 'Delete'
mkInputDir :: OsPath -> OsPath
mkInputDir testModName = mkDirPrefix testModName </> [osp|input|]

-- | Takes a name like 'Delete'
mkOutputDir :: OsPath -> OsPath
mkOutputDir testModName = mkDirPrefix testModName </> [osp|output|]

mkDirPrefix :: OsPath -> OsPath
mkDirPrefix p =
  [ospPathSep|lib/cli/test/functional/Test/Cli/Functional|] </> p
