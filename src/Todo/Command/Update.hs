module Todo.Command.Update
  ( -- * Setters
    setTaskDeadline,
    setTaskDescription,
    setTaskId,
    setTaskPriority,
    setTaskStatus,

    -- * Exceptions
    FoundGroupNotSingleE (..),
  )
where

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Todo.Configuration.Core
  ( CoreConfig (colorSwitch, index, unicodeSwitch),
    CoreConfigMerged,
  )
import Todo.Data.Task (SomeTask, someTaskStatusATraversal)
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskPriority (TaskPriority)
import Todo.Data.TaskStatus (TaskStatus, _Blocked, _BlockerId)
import Todo.Data.Timestamp (Timestamp)
import Todo.Index (Index, TaskIdNotFoundE (MkTaskIdNotFoundE))
import Todo.Index qualified as Index
import Todo.Prelude
import Todo.Render qualified as Render
import Todo.Utils (MatchResult (MatchFailure, MatchPartial, MatchSuccess))
import Todo.Utils qualified as Utils

setTaskDeadline ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadTerminal m,
    MonadThrow m,
    MonadTime m
  ) =>
  CoreConfigMerged ->
  -- | The task id.
  TaskId ->
  -- | The task deadline.
  Timestamp ->
  m ()
setTaskDeadline = setTaskValue updateDeadline
  where
    updateDeadline taskId newDeadline index = do
      let mSetResult = Index.reallyUnsafeSetTaskValue #deadline taskId (Just newDeadline) index
      liftMatchSuccessM taskId mSetResult

setTaskDescription ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadTerminal m,
    MonadThrow m,
    MonadTime m
  ) =>
  CoreConfigMerged ->
  -- | The task id.
  TaskId ->
  -- | The task description.
  Text ->
  m ()
setTaskDescription = setTaskValue updateDescription
  where
    updateDescription taskId newDesc index = do
      let mSetResult = Index.reallyUnsafeSetTaskValue #description taskId (Just newDesc) index
      liftMatchSuccessM taskId mSetResult

setTaskId ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadTerminal m,
    MonadThrow m,
    MonadTime m
  ) =>
  CoreConfigMerged ->
  -- | The current task id.
  TaskId ->
  -- | The new task id.
  TaskId ->
  m ()
setTaskId = setTaskValue updateId
  where
    updateId oldTaskId newId index = do
      mSetResult <-
        Index.setSomeTaskValueMappedValidate
          updateBlockers
          #taskId
          oldTaskId
          newId
          index
      liftJustM oldTaskId mSetResult
      where
        updateBlockers :: Index -> Index
        updateBlockers = over' indexBlockerIdTraversal g
          where
            -- Targets all blocking ids
            indexBlockerIdTraversal :: Traversal' Index TaskId
            indexBlockerIdTraversal =
              Index.indexTraversal
                % someTaskStatusATraversal
                % _Blocked
                % Utils.neSetTraversal
                % _BlockerId

            g tid =
              if tid == oldTaskId
                then newId
                else tid

setTaskPriority ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadTerminal m,
    MonadThrow m,
    MonadTime m
  ) =>
  CoreConfigMerged ->
  -- | The task id.
  TaskId ->
  -- | The task status.
  TaskPriority ->
  m ()
setTaskPriority = setTaskValue updatePriority
  where
    updatePriority taskId newPriority index = do
      let mSetResult = Index.reallyUnsafeSetSomeTaskValue #priority taskId newPriority index
      liftJustM taskId mSetResult

setTaskStatus ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadTerminal m,
    MonadThrow m,
    MonadTime m
  ) =>
  CoreConfigMerged ->
  -- | The task id.
  TaskId ->
  -- | The task status.
  TaskStatus ->
  m ()
setTaskStatus = setTaskValue updateStatus
  where
    updateStatus taskId newStatus index = do
      mSetResult <- Index.setSomeTaskValueValidate #status taskId newStatus index
      liftJustM taskId mSetResult

setTaskValue ::
  forall m a.
  ( HasCallStack,
    MonadFileWriter m,
    MonadTerminal m,
    MonadTime m
  ) =>
  (TaskId -> a -> Index -> m (Index, SomeTask)) ->
  CoreConfigMerged ->
  -- | The task id.
  TaskId ->
  -- | The task value.
  a ->
  m ()
setTaskValue f coreConfig taskId newValue = do
  (newIndex, modifiedTask) <- f taskId newValue index

  Index.writeIndex newIndex
  putTextLn "Successfully updated task:\n"

  rendered <- Render.renderOne color unicode modifiedTask

  putTextLn
    $ TL.toStrict
    $ TLB.toLazyText rendered
  where
    color = coreConfig.colorSwitch
    index = coreConfig.index
    unicode = coreConfig.unicodeSwitch

liftJustM :: (HasCallStack, MonadThrow m) => TaskId -> Maybe a -> m a
liftJustM _ (Just x) = pure x
liftJustM taskId Nothing = throwM $ MkTaskIdNotFoundE taskId

liftMatchSuccessM :: (HasCallStack, MonadThrow m) => TaskId -> MatchResult s a -> m (Tuple2 s a)
liftMatchSuccessM _ (MatchSuccess x y) = pure (x, y)
liftMatchSuccessM taskId MatchFailure = throwM $ MkTaskIdNotFoundE taskId
liftMatchSuccessM taskId (MatchPartial _) = throwM $ MkFoundGroupNotSingleE taskId

newtype FoundGroupNotSingleE = MkFoundGroupNotSingleE TaskId
  deriving stock (Eq, Show)

instance Exception FoundGroupNotSingleE where
  displayException (MkFoundGroupNotSingleE taskId) =
    mconcat
      [ "Found a group task with id '",
        unpack taskId.unTaskId,
        "', but wanted a single task."
      ]
