{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides "safe" alternatives to some of the Index's unsafe functions.
-- For instance, rather than Index's reallyUnsafeInsert, we have insert,
-- which requires a proof that the new TaskId does not exist within the
-- Index.
module Todo.Index.Safe
  ( -- * Types
    IndexWithData (..),
    SingleTaskId (..),
    GroupTaskId (..),

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
  deriving stock (Eq, Functor, Show)

-- | Newtype for a single task id.
newtype SingleTaskId = MkSingleTaskId {unSingleTaskId :: TaskId}
  deriving stock (Eq, Show)

-- | Newtype for a group task id.
newtype GroupTaskId = MkGroupTaskId {unGroupTaskId :: TaskId}
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
------------------------------------ Task ID -----------------------------------
--------------------------------------------------------------------------------

instance HasField "groupTaskId" (IndexWithData GroupTaskId) GroupTaskId where
  getField x = x.extraData

instance HasField "singleTaskId" (IndexWithData SingleTaskId) SingleTaskId where
  getField x = x.extraData

instance HasField "groupTaskId" (IndexWithData (Tuple2 a GroupTaskId)) GroupTaskId where
  getField x = groupId
    where
      (_, groupId) = x.extraData

instance HasField "singleTaskId" (IndexWithData (Tuple2 SingleTaskId a)) SingleTaskId where
  getField x = taskId
    where
      (taskId, _) = x.extraData

--------------------------------------------------------------------------------
----------------------------------- SomeTask -----------------------------------
--------------------------------------------------------------------------------

instance HasField "task" (IndexWithData SomeTask) SomeTask where
  getField x = x.extraData

instance HasField "task" (IndexWithData (Tuple2 SomeTask a)) SomeTask where
  getField x = task
    where
      (task, _) = x.extraData

--------------------------------------------------------------------------------
---------------------------------- Predicates ----------------------------------
--------------------------------------------------------------------------------

-- | Predicate for a TaskId not in Index.
data TaskIdNotMember

instance
  (HasField "singleTaskId" (IndexWithData a) SingleTaskId) =>
  Predicate TaskIdNotMember (IndexWithData a)
  where
  validate p x =
    if isMember
      then Just $ RefineSomeException (typeRep p) (toException $ MkDuplicateIdE taskId)
      else Nothing
    where
      isMember = Index.member taskId x.index

      taskId = x.singleTaskId.unSingleTaskId

-- | Predicate for a TaskId corresponding to a TaskGroup in the Index.
data GroupIdMember

instance
  (HasField "groupTaskId" (IndexWithData a) GroupTaskId) =>
  Predicate GroupIdMember (IndexWithData a)
  where
  validate p x = case Index.lookup groupId x.index of
    Nothing ->
      Just $ RefineSomeException (typeRep p) (toException $ MkTaskIdNotFoundE groupId)
    Just (SingleTask _) ->
      Just
        $ RefineOtherException
          (typeRep p)
          ( mconcat
              [ "The task id '",
                groupId.unTaskId,
                "' exists in the index but is a single task id, not a group."
              ]
          )
    Just (MultiTask _) -> Nothing
    where
      groupId = x.groupTaskId.unGroupTaskId

--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------

-- | Safely maps a RIndexWithNewId to a RIndexWithNewTask.
addTaskToId ::
  Refined TaskIdNotMember (IndexWithData SingleTaskId) ->
  (TaskId -> SomeTask) ->
  Refined TaskIdNotMember (IndexWithData SomeTask)
addTaskToId r onTask = RE.reallyUnsafeLiftR toTask r
  where
    toTask :: IndexWithData SingleTaskId -> IndexWithData SomeTask
    toTask x = MkIndexWithData x.index (onTask x.singleTaskId.unSingleTaskId)

-- | Composes our proofs together. Unsafe in the sense that we assume the
-- parameters share the same index.
reallyUnsafeJoinIds ::
  Refined TaskIdNotMember (IndexWithData SingleTaskId) ->
  Refined GroupIdMember (IndexWithData GroupTaskId) ->
  Refined (GroupIdMember && TaskIdNotMember) (IndexWithData (Tuple2 SingleTaskId GroupTaskId))
reallyUnsafeJoinIds (MkRefined r1) (MkRefined r2) =
  RUnsafe.reallyUnsafeRefine
    $ MkIndexWithData
      { index = r1.index,
        extraData = (r1.singleTaskId, r2.groupTaskId)
      }

-- | Safely maps a RIndexWithNewIdAndGroupId to a RIndexWithNewTaskAndGroupId.
addTaskToIdAndGroupId ::
  Refined (GroupIdMember && TaskIdNotMember) (IndexWithData (Tuple2 SingleTaskId GroupTaskId)) ->
  (TaskId -> SomeTask) ->
  Refined (GroupIdMember && TaskIdNotMember) (IndexWithData (Tuple2 SomeTask GroupTaskId))
addTaskToIdAndGroupId r onTask = RE.reallyUnsafeLiftR toTask r
  where
    toTask :: IndexWithData (Tuple2 SingleTaskId GroupTaskId) -> IndexWithData (Tuple2 SomeTask GroupTaskId)
    toTask (MkIndexWithData idx (singleTaskId, groupId)) =
      MkIndexWithData idx (onTask singleTaskId.unSingleTaskId, groupId)

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
  ( HasField "groupTaskId" (IndexWithData a) GroupTaskId,
    HasField "task" (IndexWithData a) SomeTask,
    p :=> GroupIdMember,
    p :=> TaskIdNotMember
  ) =>
  Refined p (IndexWithData a) ->
  Index
insertAtGroupId (MkRefined x) =
  Index.reallyUnsafeInsertAtTaskId
    x.groupTaskId.unGroupTaskId
    x.task
    x.index
