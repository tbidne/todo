module Todo.Index.Optics
  ( -- * Lenses
    pathLens,
    taskListLens,

    -- * Indexed
    ix,
    at,

    -- * Traversals
    indexIdStatusFold,
    indexPredTraversal,
    indexTraversal,

    -- * Misc
    unverifyGetter,
  )
where

import Todo.Data.Task (SomeTask)
import Todo.Data.Task.Optics qualified as Task
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskStatus (TaskStatus)
import Todo.Index.Internal (Index (UnsafeIndex), Index𝕌)
import Todo.Index.Internal qualified as Internal
import Todo.Prelude
import Todo.Utils qualified as Utils

-- NOTE: [Optics Safety]
--
-- We need to ensure that our optics do not allow violating our invariants.
-- This means:
--
-- 1. For optics that can change the index (e.g. Lenses, general traversals),
--    the result must be Index𝕌, and the source either (Index s)
--    (preferred where possible) or Index𝕌.
--
-- 2. Optics that only retrieve values (folds) can either be type-preserving
--    on (Index s) (preferred) or result in Index𝕌. In particular,
--    we cannot allow transforming Index𝕍 to Index𝕌.
--
-- Usage is usually straightforward, though we sometimes have to manually
-- unverify the index first.

-- | Path lens.
--
-- This appears to violate our methodology in NOTE: [Optics Safety], but
-- modifying the path does not impact any invariants, so it is fine.
pathLens :: Lens' (Index s) OsPath
pathLens =
  lensVL
    $ \f
       (UnsafeIndex _taskList _path) ->
        fmap
          (UnsafeIndex _taskList)
          (f _path)
{-# INLINE pathLens #-}

-- | Lens for task list.
taskListLens :: Lens (Index s) Index𝕌 (Seq SomeTask) (Seq SomeTask)
taskListLens =
  lensVL
    $ \f
       (UnsafeIndex _taskList _path) ->
        fmap
          (`UnsafeIndex` _path)
          (f _taskList)
{-# INLINE taskListLens #-}

-- | Indexed affine traversal.
ix :: TaskId -> AffineTraversal (Index s) Index𝕌 SomeTask SomeTask
ix taskId =
  atraversal
    (\idx -> mToE (Internal.unverify idx) $ Internal.lookup taskId idx)
    (\idx -> Internal.replaceAtId taskId idx . Just)
{-# INLINE ix #-}

-- | Indexed lens.
at :: TaskId -> Lens (Index s) Index𝕌 (Maybe SomeTask) (Maybe SomeTask)
at taskId = lens (Internal.lookup taskId) (Internal.replaceAtId taskId)
{-# INLINE at #-}

-- | Retrieves all ids and statuses.
indexIdStatusFold :: Fold (Index s) (Tuple2 TaskId TaskStatus)
indexIdStatusFold =
  unverifyGetter % indexTraversal % Task.someTaskIdStatusGetter

-- | Traversal for every task that satisfies the predicate.
indexPredTraversal ::
  (SomeTask -> Bool) ->
  Traversal (Index s) Index𝕌 SomeTask SomeTask
indexPredTraversal p =
  taskListLens
    % Utils.seqTraversal
    % Task.someTaskPredTraversal p

-- | Traversal across all tasks.
indexTraversal :: Traversal (Index s) Index𝕌 SomeTask SomeTask
indexTraversal = indexPredTraversal (const True)

-- | Drops verification.
unverifyGetter :: Getter (Index s) Index𝕌
unverifyGetter = to Internal.unverify
