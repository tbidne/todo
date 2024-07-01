{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides "safe" alternatives to some of the Index's unsafe functions.
-- For instance, rather than Index's reallyUnsafeInsert, we have insert,
-- which requires a proof that the new TaskId does not exist within the
-- Index.
module Todo.Index.Safe
  ( -- * Types
    IndexWithData (..),

    -- ** Aliases

    -- *** Task Ids
    IndexWithId,
    IndexWithIdAndGroupId,
    RIndexWithNewId,
    RIndexWithGroupId,
    RIndexWithNewIdAndGroupId,

    -- *** SomeTask
    IndexWithTask,
    IndexWithTaskAndGroupId,
    RIndexWithNewTask,
    RIndexWithNewTaskAndGroupId,

    -- * Predicates
    TaskIdNotMember,
    GroupIdMember,

    -- * Functions
    insert,
    insertAtGroupId,
    reallyUnsafeJoinIds,

    -- * Misc
    addTaskToId,
    addTaskToIdAndGroupId,
  )
where

import Data.Typeable (typeRep)
import GHC.Records (HasField (getField))
import Refined
  ( Predicate (validate),
    RefineException (RefineOtherException, RefineSomeException),
    Refined,
    type (&&),
  )
import Refined.Extras ((:=>))
import Refined.Extras qualified as RE
import Refined.Extras.Utils (pattern MkRefined)
import Refined.Unsafe qualified as RUnsafe
import Todo.Data.Task (SomeTask (MultiTask, SingleTask))
import Todo.Data.TaskId (TaskId (unTaskId))
import Todo.Index
  ( DuplicateIdE (MkDuplicateIdE),
    Index,
    TaskIdNotFoundE (MkTaskIdNotFoundE),
  )
import Todo.Index qualified as Index
import Todo.Prelude

-- | Index with some extra data.
data IndexWithData a = MkIndexWithData
  { index :: Index,
    extraData :: a
  }

--------------------------------------------------------------------------------
------------------------------------ Task ID -----------------------------------
--------------------------------------------------------------------------------

-- | Index with TaskId
type IndexWithId = IndexWithData TaskId

-- | Index with TaskId and group TaskId
type IndexWithIdAndGroupId = IndexWithData (Tuple2 TaskId TaskId)

-- | Refined Index with disjoint TaskId.
type RIndexWithNewId = Refined TaskIdNotMember IndexWithId

-- | Refined Index with extant group TaskId
type RIndexWithGroupId = Refined GroupIdMember IndexWithId

-- | Refined index with disjoint TaskId and group TaskId
type RIndexWithNewIdAndGroupId = Refined (GroupIdMember && TaskIdNotMember) IndexWithIdAndGroupId

instance HasField "groupId" IndexWithId TaskId where
  getField x = x.extraData

instance HasField "taskId" IndexWithId TaskId where
  getField x = x.extraData

instance HasField "groupId" IndexWithIdAndGroupId TaskId where
  getField x = groupId
    where
      (_, groupId) = x.extraData

instance HasField "taskId" IndexWithIdAndGroupId TaskId where
  getField x = taskId
    where
      (taskId, _) = x.extraData

--------------------------------------------------------------------------------
----------------------------------- SomeTask -----------------------------------
--------------------------------------------------------------------------------

-- | Index with SomeTask
type IndexWithTask = IndexWithData SomeTask

-- | Index with SomeTask and group TaskId
type IndexWithTaskAndGroupId = IndexWithData (Tuple2 SomeTask TaskId)

-- | Refined Index with disjoint Task
type RIndexWithNewTask = Refined TaskIdNotMember IndexWithTask

-- | Refined Index with disjoint Task and extant group TaskId.
type RIndexWithNewTaskAndGroupId = Refined (GroupIdMember && TaskIdNotMember) IndexWithTaskAndGroupId

instance HasField "task" IndexWithTask SomeTask where
  getField x = x.extraData

instance HasField "taskId" IndexWithTask TaskId where
  getField x = x.extraData.taskId

instance HasField "groupId" IndexWithTaskAndGroupId TaskId where
  getField x = groupId
    where
      (_, groupId) = x.extraData

instance HasField "task" IndexWithTaskAndGroupId SomeTask where
  getField x = st
    where
      (st, _) = x.extraData

instance HasField "taskId" IndexWithTaskAndGroupId TaskId where
  getField x = st.taskId
    where
      (st, _) = x.extraData

--------------------------------------------------------------------------------
---------------------------------- Predicates ----------------------------------
--------------------------------------------------------------------------------

-- | Predicate for a TaskId not in Index.
data TaskIdNotMember

instance
  (HasField "taskId" (IndexWithData a) TaskId) =>
  Predicate TaskIdNotMember (IndexWithData a)
  where
  validate p x =
    if isMember
      then Just $ RefineSomeException (typeRep p) (toException $ MkDuplicateIdE x.taskId)
      else Nothing
    where
      isMember = Index.member x.taskId x.index

-- | Predicate for a TaskId corresponding to a TaskGroup in the Index.
data GroupIdMember

instance
  (HasField "groupId" (IndexWithData a) TaskId) =>
  Predicate GroupIdMember (IndexWithData a)
  where
  validate p x = case Index.lookup x.groupId x.index of
    Nothing ->
      Just $ RefineSomeException (typeRep p) (toException $ MkTaskIdNotFoundE x.groupId)
    Just (SingleTask _) ->
      Just
        $ RefineOtherException
          (typeRep p)
          ("The task id '" <> x.groupId.unTaskId <> "' exists in the index but is a single task id, not a group.")
    Just (MultiTask _) -> Nothing

--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------

-- | Safely maps a RIndexWithNewId to a RIndexWithNewTask.
addTaskToId ::
  RIndexWithNewId ->
  (TaskId -> SomeTask) ->
  RIndexWithNewTask
addTaskToId r onTask = RE.unsafeLiftR toTask r
  where
    toTask :: IndexWithId -> IndexWithTask
    toTask x = MkIndexWithData x.index (onTask x.taskId)

-- | Composes our proofs together. Unsafe in the sense that we assume the
-- parameters share the same index.
reallyUnsafeJoinIds ::
  RIndexWithNewId ->
  RIndexWithGroupId ->
  RIndexWithNewIdAndGroupId
reallyUnsafeJoinIds (MkRefined r1) (MkRefined r2) =
  RUnsafe.reallyUnsafeRefine
    $ MkIndexWithData
      { index = r1.index,
        extraData = (r1.taskId, r2.groupId)
      }

-- | Safely maps a RIndexWithNewIdAndGroupId to a RIndexWithNewTaskAndGroupId.
addTaskToIdAndGroupId ::
  RIndexWithNewIdAndGroupId ->
  (TaskId -> SomeTask) ->
  RIndexWithNewTaskAndGroupId
addTaskToIdAndGroupId r onTask = RE.unsafeLiftR toTask r
  where
    toTask :: IndexWithIdAndGroupId -> IndexWithTaskAndGroupId
    toTask (MkIndexWithData idx (taskId, groupId)) =
      MkIndexWithData idx (onTask taskId, groupId)

-- | Inserts a task into the index. Requires a proof that the TaskId does
-- not exist within the index.
insert ::
  ( HasField "task" (IndexWithData a) SomeTask,
    TaskIdNotMember :=> p
  ) =>
  Refined p (IndexWithData a) ->
  Index
insert (MkRefined x) = Index.reallyUnsafeInsert x.task x.index

-- | Inserts a task into the index. Requires prooff that the TaskId does
-- not exist within the index, and that the group id corresponds to an
-- extant task group.
insertAtGroupId ::
  ( HasField "groupId" (IndexWithData a) TaskId,
    HasField "task" (IndexWithData a) SomeTask,
    p :=> GroupIdMember,
    p :=> TaskIdNotMember
  ) =>
  Refined p (IndexWithData a) ->
  Index
insertAtGroupId (MkRefined x) =
  Index.reallyUnsafeInsertAtTaskId
    x.groupId
    x.task
    x.index
