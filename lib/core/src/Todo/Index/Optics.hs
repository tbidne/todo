module Todo.Index.Optics
  ( indexIdStatusFold,
    indexPredTraversal,
    indexTraversal,
  )
where

import Todo.Data.Task (SomeTask)
import Todo.Data.Task.Optics qualified as Task
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskStatus (TaskStatus)
import Todo.Index (Index)
import Todo.Prelude
import Todo.Utils qualified as Utils

-- | Retrieves all ids.
indexIdStatusFold :: Fold (Index s) (Tuple2 TaskId TaskStatus)
indexIdStatusFold = indexTraversal % Task.someTaskIdStatusGetter

-- | Traversal for every task that satisfies the predicate.
indexPredTraversal :: (SomeTask -> Bool) -> Traversal' (Index s) SomeTask
indexPredTraversal p =
  #taskList
    % Utils.seqTraversal
    % Task.someTaskPredTraversal p

-- | Traversal across all tasks.
indexTraversal :: Traversal' (Index s) SomeTask
indexTraversal = indexPredTraversal (const True)
