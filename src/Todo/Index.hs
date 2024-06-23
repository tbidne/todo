module Todo.Index
  ( -- * Types
    Index (unIndex),

    -- * Creation
    readIndex,
    readTaskList,
    fromList,

    -- * Membership
    member,
    (∈),
    notMember,
    (∉),

    -- * Insertion
    reallyUnsafeInsert,

    -- * Elimination
    writeIndex,
    toList,

    -- * Exceptions,
    BlockedIdRefE (..),
    DuplicateIdE (..),
  )
where

import Data.Aeson (AesonException (AesonException))
import Data.Aeson qualified as Asn
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Set (Set)
import Data.Set qualified as Set
import Effects.FileSystem.FileReader (MonadFileReader (readBinaryFile))
import Todo.Data.Task
  ( SomeTask (MultiTask, SingleTask),
    Task (status, taskId),
    TaskGroup (subtasks, taskId),
  )
import Todo.Data.Task.TaskId (TaskId (unTaskId))
import Todo.Data.Task.TaskId qualified as TaskId
import Todo.Data.Task.TaskStatus (TaskStatus (Blocked))
import Todo.Index.Internal (Index (UnsafeIndex, unIndex))
import Todo.Prelude hiding (toList)

-- | Reads the file to an 'Index'.
readIndex ::
  ( HasCallStack,
    MonadFileReader m,
    MonadThrow m
  ) =>
  OsPath ->
  m Index
readIndex = readTaskList >=> fromList

-- | Writes the index to the path.
writeIndex ::
  ( HasCallStack,
    MonadFileWriter m
  ) =>
  OsPath ->
  Index ->
  m ()
writeIndex path index = writeBinaryFile path encoded
  where
    encoded = BSL.toStrict $ Asn.encode index.unIndex

-- | Reads the file to a task list.
readTaskList ::
  ( HasCallStack,
    MonadFileReader m,
    MonadThrow m
  ) =>
  OsPath ->
  m (List SomeTask)
readTaskList path = do
  contents <- readBinaryFile path
  case Asn.eitherDecodeStrict contents of
    Right xs -> pure xs
    Left err -> throwM $ AesonException err

-- | Inserts a task into the index. Note that this does __not__ check that
-- the id is not a duplicate i.e. this should only be used when the check has
-- already been performed.
reallyUnsafeInsert :: SomeTask -> Index -> Index
reallyUnsafeInsert task (UnsafeIndex idx) = UnsafeIndex $ task : idx

toList :: Index -> List SomeTask
toList = (.unIndex)

-- | Error for two tasks t1.name and t2.name having the same id.
newtype DuplicateIdE = MkDuplicateIdE TaskId
  deriving stock (Eq, Show)

instance Exception DuplicateIdE where
  displayException (MkDuplicateIdE id) =
    mconcat
      [ "Found duplicate tasks with id '",
        unpack id.unTaskId,
        "'."
      ]

-- | Error for a task t1 referencing task ids that do not exist.
data BlockedIdRefE
  = MkBlockedIdRefE
      -- | t1.id
      TaskId
      -- | t1.refIds
      (NESeq TaskId)
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
      toText t = "'" <> t.unTaskId <> "'"
      displayIds = TaskId.neSeqToTextCustom toText refIds

type IdSet = Set TaskId

-- | Map from a task's t's Id to its text name and referenced id (Blocked)
type IdRefMap = Map TaskId (NESeq TaskId)

type Acc = Tuple2 IdSet IdRefMap

-- NOTE: [Index representation]
--
-- We do not do any actual parsing here. We merely validate that the list
-- does not have any duplicate ids, and that all id references exist.
-- The fact is, List is already pretty much what we need: an unordered
-- list of tasks.
--
-- Something more structured that supports efficient operations
-- would be nice _in theory_ (e.g. a Map TaskId Task), but our list already
-- has structure in the form of Task Groups. We need to preserve the task group
-- structure, so trying to impose some order on top is not trivial.
--
-- We therefore keep the simple list for now, as anything more complex is
-- likely not worth it until there is a demonstrable need.

-- | Parses a list into an Index. Throws errors for duplicate ids or id
-- references that do not exist (i.e. Blocked status).
fromList :: forall m. (HasCallStack, MonadThrow m) => List SomeTask -> m Index
fromList xs = do
  (foundKeys, blockedKeys) <- mkMaps

  forWithKey_ blockedKeys $ \taskId refIds -> do
    let nonExtantRefIds = NESeq.filter (`Set.notMember` foundKeys) refIds
    case nonExtantRefIds of
      Empty -> pure ()
      (r :<| rs) -> throwM $ MkBlockedIdRefE taskId (r :<|| rs)

  pure $ UnsafeIndex xs
  where
    mkMaps = foldr go (pure (Set.empty, Map.empty)) xs

    go :: (HasCallStack) => SomeTask -> m Acc -> m Acc
    go st macc = do
      (foundKeys, blockedKeys) <- macc

      case st of
        SingleTask t -> do
          if Set.notMember t.taskId foundKeys
            then pure ()
            else
              throwM $ MkDuplicateIdE t.taskId

          foundKeys' <- updateTaskMap (SingleTask t) foundKeys

          let blockedKeys' = case t.status of
                Blocked tids -> Map.insert t.taskId tids blockedKeys
                _ -> blockedKeys

          pure (foundKeys', blockedKeys')
        MultiTask t -> do
          -- Parse each subtask with empty maps, otherwise each sub acc would
          -- include the upstream tasks, which would then fail the duplicate
          -- check in concatAccs
          subtaskAccs <- traverse (`go` pure (Set.empty, Map.empty)) t.subtasks

          -- Add upstream maps to list
          acc <- macc
          let allAccs = acc :<|| NESeq.toSeq subtaskAccs

          -- combine accs stuff
          (foundKeysAccs, blockedKeysAccs) <- concatAccs allAccs

          -- check duplicate keys
          foundKeysAccs' <- updateTaskMap (MultiTask t) foundKeysAccs

          pure (foundKeysAccs', blockedKeysAccs)

    concatAccs :: (HasCallStack) => NESeq Acc -> m Acc
    concatAccs = foldl' f (pure (Set.empty, Map.empty))
      where
        f :: m Acc -> Acc -> m Acc
        f macc (foundKeys, blockedKeys) = do
          (foundKeysAcc, blockedKeysAcc) <- macc

          let intersect = Set.intersection foundKeys foundKeysAcc

          case Set.toList intersect of
            [] -> pure ()
            (dupId : _) -> throwM $ MkDuplicateIdE dupId

          let blockedKeys' = Map.union blockedKeys blockedKeysAcc
              foundKeys' = Set.union foundKeys foundKeysAcc

          pure (foundKeys', blockedKeys')

    updateTaskMap ::
      (HasCallStack) =>
      SomeTask ->
      IdSet ->
      m IdSet
    updateTaskMap val s =
      if Set.notMember val.taskId s
        then pure $ Set.insert val.taskId s
        else throwM $ MkDuplicateIdE val.taskId

-- | Looks up the TaskId in the Index.
lookup :: TaskId -> Index -> Maybe SomeTask
lookup taskId index = foldMapAlt go idx
  where
    go (SingleTask t) =
      if t.taskId == taskId
        then Just $ SingleTask t
        else Nothing
    go (MultiTask tg) =
      if tg.taskId == taskId
        then Just $ MultiTask tg
        else foldMapAlt go tg.subtasks

    idx = index.unIndex

-- | Returns 'True' iff the TaskId exists in the index.
member :: TaskId -> Index -> Bool
member taskId = isJust . lookup taskId

-- | Operator alias for 'member'. U+2216.
(∈) :: TaskId -> Index -> Bool
(∈) = member

infix 4 ∈

-- | Negation of 'member'.
notMember :: TaskId -> Index -> Bool
notMember taskId = not . member taskId

-- | Negation of '(∈)'. U+2209.
--
-- @since 0.1
(∉) :: TaskId -> Index -> Bool
(∉) = notMember

infix 4 ∉

forWithKey :: (Applicative f) => Map k a -> (k -> a -> f b) -> f (Map k b)
forWithKey = flip Map.traverseWithKey

forWithKey_ :: (Applicative f) => Map k a -> (k -> a -> f b) -> f ()
forWithKey_ mp = void . forWithKey mp
