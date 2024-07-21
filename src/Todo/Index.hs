module Todo.Index
  ( -- * Types
    Index,

    -- * Creation
    readIndex,
    readTaskList,
    fromList,

    -- * Lookup
    Internal.lookup,

    -- * Membership
    member,
    (∈),
    notMember,
    (∉),

    -- * Insertion
    reallyUnsafeInsert,
    reallyUnsafeInsertAtTaskId,

    -- * Update
    delete,
    reallyUnsafeSetSomeTaskValue,
    setSomeTaskValueValidate,
    setSomeTaskValueMappedValidate,
    reallyUnsafeSetTaskValue,

    -- * Elimination
    writeIndex,
    toList,
    getAllIds,

    -- * Predicates
    filterTopLevel,
    filterOnIds,
    partitionTaskIds,
    partition,

    -- * Misc
    getBlockingIds,

    -- ** Optics
    indexTraversal,
    indexPredTraversal,
  )
where

import Data.Aeson (AesonException (AesonException))
import Data.Aeson qualified as Asn
import Data.Aeson.Encode.Pretty qualified as AsnPretty
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable qualified as F
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Set.NonEmpty (pattern IsEmpty, pattern IsNonEmpty)
import Data.Set.NonEmpty qualified as NESet
import Effects.FileSystem.FileReader (MonadFileReader (readBinaryFile))
import Optics.Core qualified as O
import Todo.Data.Task
  ( SingleTask (..),
    SomeTask (SomeTaskGroup, SomeTaskSingle),
    TaskGroup (status, subtasks, taskId),
    _SomeTaskGroup,
    _SomeTaskSingle,
  )
import Todo.Data.Task qualified as Task
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskStatus (TaskStatus (Blocked))
import Todo.Data.TaskStatus qualified as TaskStatus
import Todo.Exception
  ( BlockedIdRefE (MkBlockedIdRefE),
    DeleteE (DeleteRefId, DeleteTaskIdNotFound),
    DuplicateIdE (MkDuplicateIdE),
    TaskIdNotFoundE (MkTaskIdNotFoundE),
  )
import Todo.Index.Internal (Index (UnsafeIndex))
import Todo.Index.Internal qualified as Internal
import Todo.Prelude hiding (filter, toList)
import Todo.Utils (MatchResult)
import Todo.Utils qualified as Utils

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
  m (Tuple2 OsPath (Seq SomeTask))
readTaskList path = do
  contents <- readBinaryFile path
  case Asn.eitherDecodeStrict contents of
    Right xs -> pure (path, xs)
    Left err -> throwM $ AesonException err

-- | Inserts a task into the index. Note that this does __not__ check that
-- the id is not a duplicate i.e. this should only be used when the check has
-- already been performed.
reallyUnsafeInsert :: SomeTask -> Index -> Index
reallyUnsafeInsert task = over' #taskList (task :<|)

-- | Inserts a new task t1 at the task id. Like 'reallyUnsafeInsert', this is
-- unsafe in that t1's id is not verified for uniqueness, and the given
-- task id may not exist in the index (in which case t1 will not be added).
reallyUnsafeInsertAtTaskId :: TaskId -> SomeTask -> Index -> Index
reallyUnsafeInsertAtTaskId taskId task = over' tSubtasks (task :<|)
  where
    tSubtasks :: AffineTraversal' Index (Seq SomeTask)
    tSubtasks = ix taskId % _SomeTaskGroup % #subtasks

-- | Returns a list representation of the index.
toList :: Index -> (OsPath, Seq SomeTask)
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
fromList ::
  forall m.
  ( HasCallStack,
    MonadThrow m
  ) =>
  OsPath ->
  Seq SomeTask ->
  m Index
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

          let blockedKeys' = case t.status of
                Just (Blocked blocking) -> case TaskStatus.filterBlockingIds blocking of
                  IsEmpty -> blockedKeys
                  IsNonEmpty tids -> Map.insert t.taskId tids blockedKeys
                _ -> blockedKeys

          -- Add upstream maps to list
          acc <- macc
          let allAccs = acc :<| subtaskAccs

          -- combine accs stuff
          (foundKeysAccs, blockedKeysAccs) <- concatAccs allAccs

          -- check duplicate keys
          foundKeysAccs' <- updateFoundKeys (SomeTaskGroup t) foundKeysAccs

          pure (foundKeysAccs', Map.union blockedKeysAccs blockedKeys')

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

-- | Filters the index on the task ids. Included tasks are those with an id
-- in the set, or task groups who have children in the set.
filterOnIds ::
  -- | Ids to take.
  Set TaskId ->
  -- | Index
  Index ->
  Index
filterOnIds taskIds = filterTopLevel go
  where
    go :: SomeTask -> Bool
    go (SomeTaskSingle t) = Set.member t.taskId taskIds
    go (SomeTaskGroup tg) =
      Set.member tg.taskId taskIds || F.any go tg.subtasks

-- | Filters the index on some predicate. Note this runs the predicate on
-- the top-level tasks __only__. Thus if you need to filter on subtasks,
-- the passed predicate will have todo this itself.
filterTopLevel :: (SomeTask -> Bool) -> Index -> Index
filterTopLevel p (UnsafeIndex taskList path) = UnsafeIndex (Seq.filter p taskList) path

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

type PartitionAcc = Tuple2 (Seq SomeTask) (Seq SomeTask)

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
    (tasksIn, tasksOut) = foldl' go (Empty, Empty) index.taskList

    go :: PartitionAcc -> SomeTask -> PartitionAcc
    go (accIn, accOut) st@(SomeTaskSingle _) =
      if taskPred st
        then (st :<| accIn, accOut)
        else (accIn, st :<| accOut)
    go (accIn, accOut) st@(SomeTaskGroup tg) =
      if taskPred st
        then (st :<| accIn, accOut)
        else
          let subSeq = go (Empty, Empty) <$> tg.subtasks
              (subAccIn, subAccOut) = Seq.unzip subSeq
              tg' = tg {subtasks = join subAccOut}
           in (join subAccIn <> accIn, SomeTaskGroup tg' :<| accOut)

-- | Returns 'True' iff the TaskId exists in the index.
member :: TaskId -> Index -> Bool
-- member taskId = isJust . Internal.lookup taskId
-- member taskId = is _Just . preview (ix taskId)
member taskId = is (ix taskId)

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
    (Seq SomeTask)
    (Maybe SomeTask)

-- REVIEW: With all the different collections we are using (NESet, Seq, Map,
-- etc.), it is lkely we are doing some unnecessary conversions in this
-- module. Review this to see if there is anything we can eliminate.

-- | If the task id exists in the index, returns the corresponding task and
-- the index with that task removed.
delete :: TaskId -> Index -> Either DeleteE (Tuple2 Index SomeTask)
delete taskId index@(UnsafeIndex idx path) = case foldr go (Empty, Nothing) idx of
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
      | otherwise = (st :<| tasks, mDeleted)
    go st@(SomeTaskGroup tg) (tasks, mDeleted)
      | tg.taskId == taskId = (tasks, Just st)
      | otherwise = case foldr go (Empty, Nothing) tg.subtasks of
          (_, Nothing) -> (st :<| tasks, mDeleted)
          (newSubtasks, Just deletedTask) ->
            let newTaskGroup = tg {subtasks = newSubtasks}
             in (SomeTaskGroup newTaskGroup :<| tasks, Just deletedTask)

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

-- | Attempts to set a value for the corresponding task in the index.
-- This performs __no__ validation, so it must only be used with updates that
-- cannot break internal invariants (e.g. updating the priority is safe).
--
-- For general updates, use 'setSomeTaskValueValidate'.
reallyUnsafeSetSomeTaskValue ::
  forall a.
  -- | Lens for the task value we want to set.
  Lens' SomeTask a ->
  -- | Id for the task to set.
  TaskId ->
  -- | The new value.
  a ->
  -- | The index.
  Index ->
  -- | If successful (task id exists), returns the new index and modified
  -- task.
  Maybe (Index, SomeTask)
reallyUnsafeSetSomeTaskValue taskLens taskId newA index = mSetResult
  where
    mSetResult = Utils.setPreviewNode' (ix taskId) taskLens newA index

reallyUnsafeSetTaskValue ::
  forall a.
  -- | Lens for the task value we want to set.
  Lens' SingleTask a ->
  -- | Id for the task to set.
  TaskId ->
  -- | The new value.
  a ->
  -- | The index.
  Index ->
  -- | If successful (task id exists), returns the new index and modified
  -- task.
  MatchResult Index SomeTask
reallyUnsafeSetTaskValue taskLens taskId newA index = mSetResult
  where
    mSetResult =
      Utils.setPreviewPartialNode'
        (ix taskId)
        (_SomeTaskSingle % taskLens)
        newA
        index

-- | Attempts to set a value for the corresponding task in the index.
-- We validate the result, since some updates can break invariants
-- (e.g. duplicate task ids, blocked id reference).
setSomeTaskValueValidate ::
  forall m a.
  ( MonadThrow m
  ) =>
  -- | Lens for the task value we want to set.
  Lens' SomeTask a ->
  -- | Id for the task to set.
  TaskId ->
  -- | The new value.
  a ->
  -- | The index.
  Index ->
  -- | If successful (task id exists), returns the new index and modified
  -- task.
  m (Maybe (Index, SomeTask))
setSomeTaskValueValidate = setSomeTaskValueMappedValidate identity

-- | Like 'setSomeTaskValueValidate', except we run the index mapping
-- function on the result before validation. The mapped index is returned.
setSomeTaskValueMappedValidate ::
  forall m a.
  ( MonadThrow m
  ) =>
  -- | Index mapper.
  (Index -> Index) ->
  -- | Lens for the task value we want to set.
  Lens' SomeTask a ->
  -- | Id for the task to set.
  TaskId ->
  -- | The new value.
  a ->
  -- | The index.
  Index ->
  -- | If successful (task id exists), returns the new index and modified
  -- task.
  m (Maybe (Index, SomeTask))
setSomeTaskValueMappedValidate mapIndex taskLens taskId newA index = case mSetResult of
  Nothing -> pure Nothing
  Just (newIndex, newTask) -> do
    let mappedIndex = mapIndex newIndex
    validIndex <- fromList mappedIndex.path mappedIndex.taskList
    pure $ Just (validIndex, newTask)
  where
    mSetResult = Utils.setPreviewNode' (ix taskId) taskLens newA index

-- | Retrieves all ids.
getAllIds :: Index -> List (Tuple2 TaskId TaskStatus)
getAllIds = O.toListOf (indexTraversal % idStatusGetter)
  where
    idStatusGetter :: Getter SomeTask (Tuple2 TaskId TaskStatus)
    idStatusGetter = to $ \t -> (t ^. #taskId, t ^. #status)

-- | Traversal for every task that satisfies the predicate.
indexPredTraversal :: (SomeTask -> Bool) -> Traversal' Index SomeTask
indexPredTraversal p =
  #taskList
    % Utils.seqTraversal
    % Task.someTaskPredTraversal p

-- | Traversal across all tasks.
indexTraversal :: Traversal' Index SomeTask
indexTraversal = indexPredTraversal (const True)
