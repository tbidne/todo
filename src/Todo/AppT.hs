module Todo.AppT
  ( -- * Main type
    AppT (..),
    runAppT,

    -- * IO Specialization
    AppIO,
    runAppIO,
  )
where

import Control.Monad.Trans (MonadTrans (lift))
import Effects.FileSystem.FileReader
  ( MonadFileReader (readBinaryFile),
  )
import Effects.FileSystem.FileWriter
  ( MonadFileWriter (appendBinaryFile),
  )
import Effects.FileSystem.PathReader
  ( MonadPathReader
      ( canonicalizePath,
        doesDirectoryExist,
        doesFileExist,
        doesPathExist,
        findExecutable,
        findExecutables,
        findExecutablesInDirectories,
        findFileWith,
        findFilesWith,
        getAccessTime,
        getAppUserDataDirectory,
        getCurrentDirectory,
        getDirectoryContents,
        getFileSize,
        getHomeDirectory,
        getModificationTime,
        getPermissions,
        getSymbolicLinkTarget,
        getTemporaryDirectory,
        getUserDocumentsDirectory,
        getXdgDirectory,
        getXdgDirectoryList,
        listDirectory,
        makeAbsolute,
        makeRelativeToCurrentDirectory,
        pathIsSymbolicLink
      ),
  )
import Effects.FileSystem.PathWriter
  ( MonadPathWriter
      ( copyFile,
        copyFileWithMetadata,
        copyPermissions,
        createDirectory,
        createDirectoryIfMissing,
        createDirectoryLink,
        createFileLink,
        removeDirectory,
        removeDirectoryLink,
        removeDirectoryRecursive,
        removeFile,
        removePathForcibly,
        renameDirectory,
        renameFile,
        renamePath,
        setAccessTime,
        setCurrentDirectory,
        setModificationTime,
        setPermissions,
        withCurrentDirectory
      ),
  )
import Effects.Optparse
  ( MonadOptparse (customExecParser, execParser, handleParseResult),
  )
import Effects.System.Terminal
  ( MonadTerminal
      ( getChar,
        getContents',
        getTerminalSize,
        putBinary,
        putStr,
        supportsPretty
      ),
  )
import Effects.Time
  ( MonadTime (getMonotonicTime, getSystemZonedTime),
  )
import System.Console.Haskeline (InputT)
import System.Console.Haskeline qualified as H
import Todo.Prelude

-- | Main type for running todo.
type AppT :: (Type -> Type) -> Type -> Type
newtype AppT m a = MkAppT (m a)
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadFail,
      MonadFileReader,
      MonadFileWriter,
      MonadHaskeline,
      MonadIO,
      MonadOptparse,
      MonadPathReader,
      MonadPathWriter,
      MonadTerminal,
      MonadTime,
      MonadThrow
    )

instance MonadTrans AppT where
  lift = MkAppT

runAppT :: AppT m a -> m a
runAppT (MkAppT m) = m

type AppIO = AppT (InputT IO)

runAppIO :: AppIO a -> InputT IO a
runAppIO = runAppT @(InputT IO)

runHaskelineIO :: InputT IO a -> IO a
runHaskelineIO = H.runInputT H.defaultSettings

-- Tragically we have to write these by hand since we need InputT to run
-- haskeline, and the former does not inherit our instances. The
-- OVERLAPPING pragmas are so we can derive the more general instances
-- above, which we re-use in the test suite.

instance {-# OVERLAPPING #-} MonadFileReader AppIO where
  readBinaryFile = lift . liftIO . readBinaryFile
  {-# INLINEABLE readBinaryFile #-}

instance {-# OVERLAPPING #-} MonadFileWriter AppIO where
  writeBinaryFile p = lift . liftIO . writeBinaryFile p
  {-# INLINEABLE writeBinaryFile #-}
  appendBinaryFile p = lift . liftIO . appendBinaryFile p
  {-# INLINEABLE appendBinaryFile #-}

instance {-# OVERLAPPING #-} MonadOptparse AppIO where
  execParser = liftIO . execParser
  {-# INLINEABLE execParser #-}
  customExecParser p = liftIO . customExecParser p
  {-# INLINEABLE customExecParser #-}
  handleParseResult = liftIO . handleParseResult
  {-# INLINEABLE handleParseResult #-}

instance {-# OVERLAPPING #-} MonadPathReader AppIO where
  listDirectory = liftIO . listDirectory
  {-# INLINEABLE listDirectory #-}
  getDirectoryContents = liftIO . getDirectoryContents
  {-# INLINEABLE getDirectoryContents #-}
  getCurrentDirectory = liftIO getCurrentDirectory
  {-# INLINEABLE getCurrentDirectory #-}
  getHomeDirectory = liftIO getHomeDirectory
  {-# INLINEABLE getHomeDirectory #-}
  getXdgDirectory d = liftIO . getXdgDirectory d
  {-# INLINEABLE getXdgDirectory #-}
  getXdgDirectoryList = liftIO . getXdgDirectoryList
  {-# INLINEABLE getXdgDirectoryList #-}
  getAppUserDataDirectory = liftIO . getAppUserDataDirectory
  {-# INLINEABLE getAppUserDataDirectory #-}
  getUserDocumentsDirectory = liftIO getUserDocumentsDirectory
  {-# INLINEABLE getUserDocumentsDirectory #-}
  getTemporaryDirectory = liftIO getTemporaryDirectory
  {-# INLINEABLE getTemporaryDirectory #-}
  getFileSize = liftIO . getFileSize
  {-# INLINEABLE getFileSize #-}
  canonicalizePath = liftIO . canonicalizePath
  {-# INLINEABLE canonicalizePath #-}
  makeAbsolute = liftIO . makeAbsolute
  {-# INLINEABLE makeAbsolute #-}
  makeRelativeToCurrentDirectory = liftIO . makeRelativeToCurrentDirectory
  {-# INLINEABLE makeRelativeToCurrentDirectory #-}
  doesPathExist = liftIO . doesPathExist
  {-# INLINEABLE doesPathExist #-}
  doesFileExist = liftIO . doesFileExist
  {-# INLINEABLE doesFileExist #-}
  doesDirectoryExist = liftIO . doesDirectoryExist
  {-# INLINEABLE doesDirectoryExist #-}
  findExecutable = liftIO . findExecutable
  {-# INLINEABLE findExecutable #-}
  findExecutables = liftIO . findExecutables
  {-# INLINEABLE findExecutables #-}
  findExecutablesInDirectories ps = liftIO . findExecutablesInDirectories ps
  {-# INLINEABLE findExecutablesInDirectories #-}
  findFileWith action ps fileName =
    lift $ liftIO $ findFileWith (runHaskelineIO . runAppIO . action) ps fileName
  {-# INLINEABLE findFileWith #-}
  findFilesWith action ps fileName =
    lift $ liftIO $ findFilesWith (runHaskelineIO . runAppIO . action) ps fileName
  {-# INLINEABLE findFilesWith #-}
  pathIsSymbolicLink = liftIO . pathIsSymbolicLink
  {-# INLINEABLE pathIsSymbolicLink #-}
  getSymbolicLinkTarget = liftIO . getSymbolicLinkTarget
  {-# INLINEABLE getSymbolicLinkTarget #-}
  getPermissions = liftIO . getPermissions
  {-# INLINEABLE getPermissions #-}
  getAccessTime = liftIO . getAccessTime
  {-# INLINEABLE getAccessTime #-}
  getModificationTime = liftIO . getModificationTime
  {-# INLINEABLE getModificationTime #-}

instance {-# OVERLAPPING #-} MonadPathWriter AppIO where
  createDirectory = liftIO . createDirectory
  {-# INLINEABLE createDirectory #-}
  createDirectoryIfMissing b = liftIO . createDirectoryIfMissing b
  {-# INLINEABLE createDirectoryIfMissing #-}
  removeDirectory = liftIO . removeDirectory
  {-# INLINEABLE removeDirectory #-}
  removeDirectoryRecursive = liftIO . removeDirectoryRecursive
  {-# INLINEABLE removeDirectoryRecursive #-}
  removePathForcibly = liftIO . removePathForcibly
  {-# INLINEABLE removePathForcibly #-}
  renameDirectory p = liftIO . renameDirectory p
  {-# INLINEABLE renameDirectory #-}
  setCurrentDirectory = liftIO . setCurrentDirectory
  {-# INLINEABLE setCurrentDirectory #-}
  withCurrentDirectory p action =
    lift $ liftIO $ withCurrentDirectory p (runHaskelineIO $ runAppIO action)
  {-# INLINEABLE withCurrentDirectory #-}
  removeFile = liftIO . removeFile
  {-# INLINEABLE removeFile #-}
  renameFile p = liftIO . renameFile p
  {-# INLINEABLE renameFile #-}
  renamePath p = liftIO . renamePath p
  {-# INLINEABLE renamePath #-}
  copyFile p = liftIO . copyFile p
  {-# INLINEABLE copyFile #-}
  copyFileWithMetadata p = liftIO . copyFileWithMetadata p
  {-# INLINEABLE copyFileWithMetadata #-}
  createFileLink p = liftIO . createFileLink p
  {-# INLINEABLE createFileLink #-}
  createDirectoryLink p = liftIO . createDirectoryLink p
  {-# INLINEABLE createDirectoryLink #-}
  removeDirectoryLink = liftIO . removeDirectoryLink
  {-# INLINEABLE removeDirectoryLink #-}
  setPermissions p = liftIO . setPermissions p
  {-# INLINEABLE setPermissions #-}
  copyPermissions p = liftIO . copyPermissions p
  {-# INLINEABLE copyPermissions #-}
  setAccessTime p = liftIO . setAccessTime p
  {-# INLINEABLE setAccessTime #-}
  setModificationTime p = liftIO . setModificationTime p
  {-# INLINEABLE setModificationTime #-}

instance {-# OVERLAPPING #-} MonadTerminal AppIO where
  putStr = liftIO . putStr
  {-# INLINEABLE putStr #-}
  putStrLn = liftIO . putStrLn
  {-# INLINEABLE putStrLn #-}
  putBinary = liftIO . putBinary
  {-# INLINEABLE putBinary #-}
  getChar = liftIO getChar
  {-# INLINEABLE getChar #-}
  getLine = liftIO getLine
  {-# INLINEABLE getLine #-}
  getContents' = liftIO getContents'
  {-# INLINEABLE getContents' #-}
  getTerminalSize = liftIO getTerminalSize
  {-# INLINEABLE getTerminalSize #-}
  supportsPretty = liftIO supportsPretty
  {-# INLINEABLE supportsPretty #-}

instance {-# OVERLAPPING #-} MonadTime AppIO where
  getSystemZonedTime = liftIO getSystemZonedTime
  {-# INLINEABLE getSystemZonedTime #-}
  getMonotonicTime = liftIO getMonotonicTime
  {-# INLINEABLE getMonotonicTime #-}
