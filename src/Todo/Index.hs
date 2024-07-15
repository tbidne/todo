module Todo.Index
  ( -- * Types
    Index (IndexNil, IndexCons),

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
    partitionTaskIds,
    partition,

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
import Data.Set.NonEmpty (pattern IsEmpty, pattern IsNonEmpty)
import Data.Set.NonEmpty qualified as NESet
import Data.Tuple (uncurry)
import Effects.FileSystem.FileReader (MonadFileReader (readBinaryFile))
import Todo.Data.Task
  ( SingleTask (status, taskId),
    SomeTask (SomeTaskGroup, SomeTaskSingle),
    TaskGroup (status, subtasks, taskId),
  )
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Data.TaskStatus (TaskStatus (Blocked))
import Todo.Data.TaskStatus qualified as TaskStatus
import Todo.Index.Internal (Index (UnsafeIndex))
import Todo.Prelude hiding (filter, toList)

pattern IndexNil :: OsPath -> Index
pattern IndexNil p <- UnsafeIndex [] p
  where
    IndexNil p = UnsafeIndex [] p

pattern IndexCons :: SomeTask -> List SomeTask -> OsPath -> Index
pattern IndexCons t ts p <- UnsafeIndex (t : ts) p
  where
    IndexCons t ts p = UnsafeIndex (t : ts) p

{-# COMPLETE IndexNil, IndexCons #-}

-- | Reads the file to an 'Index'.
readIndex ::
  ( HasCallStack,
    MonadFileReader m,
    MonadThrow m
  ) =>
  OsPath ->
  m Index
readIndex = readTaskList >=> uncurry fromList

-- | Writes the index to the path.
writeIndex ::
  ( HasCallStack,
    MonadFileWriter m
  ) =>
  Index ->
  m ()
writeIndex (UnsafeIndex taskList path) = writeBinaryFile path encoded
  where
    encoded =
      BSL.toStrict
        $ AsnPretty.encodePretty' AsnPretty.defConfig taskList

-- | Reads the file to a task list.
readTaskList ::
  ( HasCallStack,
    MonadFileReader m,
    MonadThrow m
  ) =>
  OsPath ->
  m (Tuple2 OsPath (List SomeTask))
readTaskList path = do
  contents <- readBinaryFile path
  case Asn.eitherDecodeStrict contents of
    Right xs -> pure (path, xs)
    Left err -> throwM $ AesonException err

-- | Inserts a task into the index. Note that this does __not__ check that
-- the id is not a duplicate i.e. this should only be used when the check has
-- already been performed.
reallyUnsafeInsert :: SomeTask -> Index -> Index
reallyUnsafeInsert task (UnsafeIndex idx path) = UnsafeIndex (task : idx) path

-- | Inserts a new task t1 at the task id. Like 'reallyUnsafeInsert', this is
-- unsafe in that t1's id is not verified for uniqueness, and the given
-- task id may not exist in the index (in which case t1 will not be added).
reallyUnsafeInsertAtTaskId :: TaskId -> SomeTask -> Index -> Index
reallyUnsafeInsertAtTaskId taskId task (UnsafeIndex idx path) =
  UnsafeIndex (foldr go [] idx) path
  where
    go :: SomeTask -> List SomeTask -> List SomeTask
    go st@(SomeTaskSingle _) acc = st : acc
    go (SomeTaskGroup tg) acc =
      if taskId == tg.taskId
        then SomeTaskGroup (tg {subtasks = task :<| tg.subtasks}) : acc
        else
          let subtasks' = foldr go [] tg.subtasks
           in SomeTaskGroup (tg {subtasks = Seq.fromList subtasks'}) : acc

-- | Returns a list representation of the index.
toList :: Index -> (OsPath, List SomeTask)
toList (UnsafeIndex taskList path) = (path, taskList)

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
fromList :: forall m. (HasCallStack, MonadThrow m) => OsPath -> List SomeTask -> m Index
fromList path xs = do
  (foundKeys, blockedKeys) <- mkMaps

  forWithKey_ blockedKeys $ \taskId refIds -> do
    let nonExtantRefIds = NESet.filter (`Set.notMember` foundKeys) refIds
    case Set.toList nonExtantRefIds of
      [] -> pure ()
      (r : rs) -> throwM $ MkBlockedIdRefE taskId (NESet.fromList (r :| rs))

  pure $ UnsafeIndex xs path
  where
    mkMaps = foldr go (pure (Set.empty, Map.empty)) xs

    go :: (HasCallStack) => SomeTask -> m FromListAcc -> m FromListAcc
    go st macc = do
      (foundKeys, blockedKeys) <- macc

      case st of
        SomeTaskSingle t -> do
          if Set.notMember t.taskId foundKeys
            then pure ()
            else
              throwM $ MkDuplicateIdE t.taskId

          foundKeys' <- updateFoundKeys (SomeTaskSingle t) foundKeys

          let blockedKeys' = case t.status of
                Blocked blocking -> case TaskStatus.filterBlockingIds blocking of
                  IsEmpty -> blockedKeys
                  IsNonEmpty tids -> Map.insert t.taskId tids blockedKeys
                _ -> blockedKeys

          pure (foundKeys', blockedKeys')
        SomeTaskGroup t -> do
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
          foundKeysAccs' <- updateFoundKeys (SomeTaskGroup t) foundKeysAccs

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
    go (SomeTaskSingle t) = Set.member t.taskId taskIds
    go (SomeTaskGroup tg) =
      Set.member tg.taskId taskIds || F.any go tg.subtasks

-- | Filters the index on some predicate.
filter :: (SomeTask -> Bool) -> Index -> Index
filter p (UnsafeIndex taskList path) = UnsafeIndex (L.filter p taskList) path

-- | Partitions the Index on the TaskId set. The left result is all tasks
-- whose ids belong in the set __and__ tasks who have a some parent in the
-- set.
--
-- The right set is all tasks whose ids do not belong in the set.
partitionTaskIds ::
  -- | TaskIds set s.
  NESet TaskId ->
  -- | Index to partition.
  Index ->
  -- | (Tasks in s, Tasks not in s)
  Tuple2 Index Index
partitionTaskIds taskIds = partition pred
  where
    pred = (`NESet.member` taskIds) . (.taskId)

type PartitionAcc = Tuple2 (List SomeTask) (List SomeTask)

-- | Partitions the Index according to the predicate @p@. If @p t@ is true
-- for some @t@ then @t@ is added to the left result, unchanged. This means
-- that, for instance, the entire task group is returned without checking
-- any children.
--
-- On the other hand, if @p t@ returns false for some task group @t@, then
-- we check subtasks. Any subtasks returning true will be added to the
-- left result, whereas @t'@ will be added to the right result, where @t'@
-- is the original group @t@ with the children that also return false.
partition ::
  -- | Predicate p.
  (SomeTask -> Bool) ->
  -- | Index to partition.
  Index ->
  -- | (p == True, p == False)
  Tuple2 Index Index
partition taskPred index =
  (UnsafeIndex tasksIn index.path, UnsafeIndex tasksOut index.path)
  where
    (tasksIn, tasksOut) = foldl' go ([], []) index.taskList

    go :: PartitionAcc -> SomeTask -> PartitionAcc
    go (accIn, accOut) st@(SomeTaskSingle _) =
      if taskPred st
        then (st : accIn, accOut)
        else (accIn, st : accOut)
    go (accIn, accOut) st@(SomeTaskGroup tg) =
      if taskPred st
        then (st : accIn, accOut)
        else
          let subSeq = go ([], []) <$> tg.subtasks
              (subAccIn, subAccOut) = L.unzip $ seqToList subSeq
              tg' = tg {subtasks = listToSeq (join subAccOut)}
           in (join subAccIn <> accIn, SomeTaskGroup tg' : accOut)

-- | Looks up the TaskId in the Index.
lookup :: TaskId -> Index -> Maybe SomeTask
lookup taskId (UnsafeIndex taskList _) = foldMapAlt go taskList
  where
    go (SomeTaskSingle t) =
      if t.taskId == taskId
        then Just $ SomeTaskSingle t
        else Nothing
    go (SomeTaskGroup tg) =
      if tg.taskId == taskId
        then Just $ SomeTaskGroup tg
        else foldMapAlt go tg.subtasks

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
delete taskId index@(UnsafeIndex idx path) = case foldr go ([], Nothing) idx of
  (_, Nothing) -> Left $ DeleteTaskIdNotFound $ MkTaskIdNotFoundE taskId
  (newIdx, Just st) ->
    let blockingIds = getBlockingIds index
     in case Map.lookup taskId blockingIds of
          Nothing -> Right (UnsafeIndex newIdx path, st)
          Just blockedIds -> Left $ DeleteRefId taskId blockedIds
  where
    go :: SomeTask -> DeleteAcc -> DeleteAcc
    go st@(SomeTaskSingle _) (tasks, mDeleted)
      | st.taskId == taskId = (tasks, Just st)
      | otherwise = (st : tasks, mDeleted)
    go st@(SomeTaskGroup tg) (tasks, mDeleted)
      | tg.taskId == taskId = (tasks, Just st)
      | otherwise = case foldr go ([], Nothing) tg.subtasks of
          (_, Nothing) -> (st : tasks, mDeleted)
          (newSubtasks, Just deletedTask) ->
            let newTaskGroup = tg {subtasks = Seq.fromList newSubtasks}
             in (SomeTaskGroup newTaskGroup : tasks, Just deletedTask)

-- | Returns a map of all blocking ids to blockees. For instance, if tasks
-- t1 and t2 are both blocked by t3, then there should be an entry:
--
-- t3 -> [t1, t2]
getBlockingIds :: Index -> Map TaskId (NESet TaskId)
getBlockingIds (UnsafeIndex idx _path) = foldl' go Map.empty idx
  where
    go :: Map TaskId (NESet TaskId) -> SomeTask -> Map TaskId (NESet TaskId)
    go mp (SomeTaskSingle t) = case t.status of
      Blocked blocking ->
        case TaskStatus.filterBlockingIds blocking of
          IsEmpty -> mp
          IsNonEmpty ids ->
            let maps = idsToMaps t.taskId ids
             in Map.unionsWith (<>) maps
      _ -> mp
    go mp (SomeTaskGroup tg) =
      let subMaps :: Seq (Map TaskId (NESet TaskId))
          subMaps = go Map.empty <$> tg.subtasks

          groupMaps :: Seq (Map TaskId (NESet TaskId))
          groupMaps = case tg.status of
            Just (Blocked blocking) ->
              case TaskStatus.filterBlockingIds blocking of
                IsEmpty -> mp :<| Empty
                IsNonEmpty ids -> mp :<| neToSeq (idsToMaps tg.taskId ids)
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
