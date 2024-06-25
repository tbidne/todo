module Todo.Index
  ( -- * Types
    Index (unIndex),

    -- * Creation
    readIndex,
    readTaskList,
    fromList,

    -- * Lookup
    lookup,

    -- * Membership
    member,
    (∈),
    notMember,
    (∉),

    -- * Insertion
    reallyUnsafeInsert,
    reallyUnsafeInsertAtTaskId,

    -- * Deletion
    delete,

    -- * Elimination
    writeIndex,
    toList,

    -- * Predicates
    filter,
    filterOnIds,

    -- * Exceptions,
    BlockedIdRefE (..),
    DuplicateIdE (..),
    DeleteE (..),
    TaskIdNotFoundE (..),

    -- * Misc
    getBlockingIds,
  )
where

import Data.Aeson (AesonException (AesonException))
import Data.Aeson qualified as Asn
import Data.Aeson.Encode.Pretty qualified as AsnPretty
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable qualified as F
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Effects.FileSystem.FileReader (MonadFileReader (readBinaryFile))
import Todo.Data.Task
  ( SomeTask (MultiTask, SingleTask),
    Task (status, taskId),
    TaskGroup (status, subtasks, taskId),
  )
import Todo.Data.TaskId (TaskId (unTaskId))
import Todo.Data.TaskId qualified as TaskId
import Todo.Data.TaskStatus (TaskStatus (Blocked))
import Todo.Index.Internal (Index (UnsafeIndex, unIndex))
import Todo.Prelude hiding (filter, toList)

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
    encoded =
      BSL.toStrict
        $ AsnPretty.encodePretty' AsnPretty.defConfig index.unIndex

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

-- | Inserts a new task t1 at the task id. Like 'reallyUnsafeInsert', this is
-- unsafe in that t1's id is not verified for uniqueness, and the given
-- task id may not exist in the index (in which case t1 will not be added).
reallyUnsafeInsertAtTaskId :: TaskId -> SomeTask -> Index -> Index
reallyUnsafeInsertAtTaskId taskId task (UnsafeIndex idx) =
  UnsafeIndex $ foldr go [] idx
  where
    go :: SomeTask -> List SomeTask -> List SomeTask
    go st@(SingleTask _) acc = st : acc
    go (MultiTask tg) acc =
      if taskId == tg.taskId
        then MultiTask (tg {subtasks = task :<| tg.subtasks}) : acc
        else
          let subtasks' = foldr go [] tg.subtasks
           in MultiTask (tg {subtasks = Seq.fromList subtasks'}) : acc

-- | Returns a list representation of the index.
toList :: Index -> List SomeTask
toList = (.unIndex)

type IdSet = Set TaskId

-- | Map from a task's t's Id to its text name and referenced id (Blocked)
type IdRefMap = Map TaskId (NESet TaskId)

type FromListAcc = Tuple2 IdSet IdRefMap

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
    let nonExtantRefIds = NESet.filter (`Set.notMember` foundKeys) refIds
    case Set.toList nonExtantRefIds of
      [] -> pure ()
      (r : rs) -> throwM $ MkBlockedIdRefE taskId (NESet.fromList (r :| rs))

  pure $ UnsafeIndex xs
  where
    mkMaps = foldr go (pure (Set.empty, Map.empty)) xs

    go :: (HasCallStack) => SomeTask -> m FromListAcc -> m FromListAcc
    go st macc = do
      (foundKeys, blockedKeys) <- macc

      case st of
        SingleTask t -> do
          if Set.notMember t.taskId foundKeys
            then pure ()
            else
              throwM $ MkDuplicateIdE t.taskId

          foundKeys' <- updateFoundKeys (SingleTask t) foundKeys

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
          let allAccs = acc :<| subtaskAccs

          -- combine accs stuff
          (foundKeysAccs, blockedKeysAccs) <- concatAccs allAccs

          -- check duplicate keys
          foundKeysAccs' <- updateFoundKeys (MultiTask t) foundKeysAccs

          pure (foundKeysAccs', blockedKeysAccs)

    concatAccs :: (HasCallStack) => Seq FromListAcc -> m FromListAcc
    concatAccs = foldl' f (pure (Set.empty, Map.empty))
      where
        f :: m FromListAcc -> FromListAcc -> m FromListAcc
        f macc (foundKeys, blockedKeys) = do
          (foundKeysAcc, blockedKeysAcc) <- macc

          let intersect = Set.intersection foundKeys foundKeysAcc

          case Set.toList intersect of
            [] -> pure ()
            (dupId : _) -> throwM $ MkDuplicateIdE dupId

          let blockedKeys' = Map.union blockedKeys blockedKeysAcc
              foundKeys' = Set.union foundKeys foundKeysAcc

          pure (foundKeys', blockedKeys')

    updateFoundKeys ::
      (HasCallStack) =>
      SomeTask ->
      IdSet ->
      m IdSet
    updateFoundKeys val s =
      if Set.notMember val.taskId s
        then pure $ Set.insert val.taskId s
        else throwM $ MkDuplicateIdE val.taskId

-- | Filters the index on the task ids.
filterOnIds ::
  -- | Ids to take.
  Set TaskId ->
  -- | Index
  Index ->
  Index
filterOnIds taskIds = filter go
  where
    go :: SomeTask -> Bool
    go (SingleTask t) = Set.member t.taskId taskIds
    go (MultiTask tg) =
      Set.member tg.taskId taskIds || F.any go tg.subtasks

-- | Filters the index on some predicate.
filter :: (SomeTask -> Bool) -> Index -> Index
filter p = UnsafeIndex . L.filter p . (.unIndex)

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
(∉) :: TaskId -> Index -> Bool
(∉) = notMember

infix 4 ∉

type DeleteAcc =
  Tuple2
    (List SomeTask)
    (Maybe SomeTask)

-- REVIEW: With all the different collections we are using (NESet, Seq, Map,
-- etc.), it is lkely we are doing some unnecessary conversions in this
-- module. Review this to see if there is anything we can eliminate.

-- | If the task id exists in the index, returns the corresponding task and
-- the index with that task removed.
delete :: TaskId -> Index -> Either DeleteE (Tuple2 Index SomeTask)
delete taskId index@(UnsafeIndex idx) = case foldr go ([], Nothing) idx of
  (_, Nothing) -> Left $ DeleteTaskIdNotFound $ MkTaskIdNotFoundE taskId
  (newIdx, Just st) ->
    let blockingIds = getBlockingIds index
     in case Map.lookup taskId blockingIds of
          Nothing -> Right (UnsafeIndex newIdx, st)
          Just blockedIds -> Left $ DeleteRefId taskId blockedIds
  where
    go :: SomeTask -> DeleteAcc -> DeleteAcc
    go st@(SingleTask _) (tasks, mDeleted)
      | st.taskId == taskId = (tasks, Just st)
      | otherwise = (st : tasks, mDeleted)
    go st@(MultiTask tg) (tasks, mDeleted)
      | tg.taskId == taskId = (tasks, Just st)
      | otherwise = case foldr go ([], Nothing) tg.subtasks of
          (_, Nothing) -> (st : tasks, mDeleted)
          (newSubtasks, Just deletedTask) ->
            let newTaskGroup = tg {subtasks = Seq.fromList newSubtasks}
             in (MultiTask newTaskGroup : tasks, Just deletedTask)

-- | Returns a map of all blocking ids to blockees. For instance, if tasks
-- t1 and t2 are both blocked by t3, then there should be an entry:
--
-- t3 -> [t1, t2]
getBlockingIds :: Index -> Map TaskId (NESet TaskId)
getBlockingIds (UnsafeIndex idx) = foldl' go Map.empty idx
  where
    go :: Map TaskId (NESet TaskId) -> SomeTask -> Map TaskId (NESet TaskId)
    go mp (SingleTask t) = case t.status of
      Blocked ids ->
        let maps = idsToMaps t.taskId ids
         in Map.unionsWith (<>) maps
      _ -> mp
    go mp (MultiTask tg) =
      let subMaps = go Map.empty <$> tg.subtasks
          groupMaps = case tg.status of
            Just (Blocked ids) -> mp :<| neToSeq (idsToMaps tg.taskId ids)
            _ -> mp :<| Empty
       in Map.unionsWith (<>) (groupMaps <> subMaps)

    idsToMaps :: TaskId -> NESet TaskId -> NonEmpty (Map TaskId (NESet TaskId))
    idsToMaps taskId ids =
      (\blockingId -> Map.singleton blockingId (NESet.singleton taskId))
        <$> NESet.toList ids

    neToSeq = Seq.fromList . NE.toList

forWithKey :: (Applicative f) => Map k a -> (k -> a -> f b) -> f (Map k b)
forWithKey = flip Map.traverseWithKey

forWithKey_ :: (Applicative f) => Map k a -> (k -> a -> f b) -> f ()
forWithKey_ mp = void . forWithKey mp

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
