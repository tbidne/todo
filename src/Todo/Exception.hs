{-# LANGUAGE CPP #-}

module Todo.Exception
  ( -- * Types
    BlockedIdRefE (..),
    ConfigNotFoundE (..),
    DeleteE (..),
    DuplicateIdE (..),
    FoundGroupNotSingleE (..),
    IndexNameLookupE (..),
    TaskIdNotFoundE (..),
    XdgIndexNotFoundE (..),

    -- * Functions
    displayExceptionSkipKnownCS,
  )
where

import Data.Proxy (Proxy (Proxy))
import Effects.Exception (ExceptionProxy (MkExceptionProxy))
import Effects.Exception qualified as Ex
import Effects.FileSystem.Utils qualified as FsUtils
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Prelude

-- | Error for two tasks t1 and t2 having the same id.
newtype DuplicateIdE = MkDuplicateIdE TaskId
  deriving stock (Eq, Show)

instance Exception DuplicateIdE where
  displayException (MkDuplicateIdE id) =
    mconcat
      [ "Found duplicate tasks with id '",
        unpack id.unTaskId,
        "'."
      ]

-- | Error for not finding a task id in the index.
newtype TaskIdNotFoundE = MkTaskIdNotFoundE TaskId
  deriving stock (Eq, Show)

instance Exception TaskIdNotFoundE where
  displayException (MkTaskIdNotFoundE taskId) =
    mconcat
      [ "Task id '",
        unpack taskId.unTaskId,
        "' not found in the index."
      ]

-- | Error for a task t1 referencing task ids that do not exist.
data BlockedIdRefE
  = MkBlockedIdRefE
      -- | t1.id
      TaskId
      -- | t1.refIds
      (NESet TaskId)
  deriving stock (Eq, Show)

instance Exception BlockedIdRefE where
  displayException (MkBlockedIdRefE id refIds) =
    mconcat
      [ "Task with id '",
        unpack id.unTaskId,
        "' references non-extant id(s): ",
        unpack displayIds,
        "."
      ]
    where
      displayIds = TaskId.taskIdsToTextQuote refIds

-- | Errors when deleting a task.
data DeleteE
  = -- | Attempted to delete an id that was not found
    DeleteTaskIdNotFound TaskIdNotFoundE
  | -- | Attempted to delete a task that is referenced by other tasks.
    DeleteRefId TaskId (NESet TaskId)
  deriving stock (Eq, Show)

instance Exception DeleteE where
  displayException (DeleteTaskIdNotFound err) = displayException err
  displayException (DeleteRefId taskId ids) =
    mconcat
      [ "Task id '",
        unpack taskId.unTaskId,
        "' is referenced by other tasks, so it cannot be deleted: ",
        unpack (TaskId.taskIdsToTextQuote ids)
      ]

-- | Error for finding a group when we wanted a single task.
newtype FoundGroupNotSingleE = MkFoundGroupNotSingleE TaskId
  deriving stock (Eq, Show)

instance Exception FoundGroupNotSingleE where
  displayException (MkFoundGroupNotSingleE taskId) =
    mconcat
      [ "Found a group task with id '",
        unpack taskId.unTaskId,
        "', but wanted a single task."
      ]

-- | Error when failing to find an index name.
data IndexNameLookupE = MkIndexNameLookupE OsPath Text
  deriving stock (Eq, Show)

instance Exception IndexNameLookupE where
  displayException (MkIndexNameLookupE path name) =
    mconcat
      [ "No index with name '",
        unpack name,
        "' found in index-legend: '",
        FsUtils.decodeOsToFpLenient path,
        "'"
      ]

-- | Error for failing to find the XDG index.
newtype XdgIndexNotFoundE = MkXdgIndexNotFoundE OsPath
  deriving stock (Eq, Show)

instance Exception XdgIndexNotFoundE where
  displayException (MkXdgIndexNotFoundE p) =
    mconcat
      [ "No index name or path was given, so we fell back to XDG config '",
        FsUtils.decodeOsToFpLenient p,
        "', but none were found."
      ]

-- | Error for config not found.
newtype ConfigNotFoundE = MkConfigNotFoundE OsPath
  deriving stock (Eq, Show)

instance Exception ConfigNotFoundE where
  displayException (MkConfigNotFoundE p) =
    mconcat
      [ "Config file not found: '",
        FsUtils.decodeOsToFpLenient p,
        "'"
      ]

-- TODO: For GHC 9.10+, consider replacing this mechanism with
-- backtraceDesired _ = False in the Exception definition.

-- | Display exception, skipping callstacks for known exceptions.
displayExceptionSkipKnownCS :: (Exception e) => e -> String
displayExceptionSkipKnownCS = skipKnownExceptions proxies

proxies :: List ExceptionProxy
proxies =
  [ MkExceptionProxy $ Proxy @BlockedIdRefE,
    MkExceptionProxy $ Proxy @ConfigNotFoundE,
    MkExceptionProxy $ Proxy @DeleteE,
    MkExceptionProxy $ Proxy @DuplicateIdE,
    MkExceptionProxy $ Proxy @FoundGroupNotSingleE,
    MkExceptionProxy $ Proxy @IndexNameLookupE,
    MkExceptionProxy $ Proxy @TaskIdNotFoundE
  ]

skipKnownExceptions :: forall e. (Exception e) => [ExceptionProxy] -> e -> String

{- ORMOLU_DISABLE -}

#if MIN_VERSION_base(4, 20, 0)
skipKnownExceptions = Ex.displayInnerMatch
#else
skipKnownExceptions = Ex.displayCSNoMatch
#endif

{- ORMOLU_ENABLE -}
