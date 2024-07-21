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

import Data.List qualified as L
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Todo.Command.Utils qualified as CUtils
import Todo.Configuration.Core
  ( CoreConfig (colorSwitch, index, unicodeSwitch),
    CoreConfigMerged,
  )
import Todo.Configuration.Data.InteractiveSwitch
  ( InteractiveSwitch
      ( InteractiveOff,
        InteractiveOn
      ),
  )
import Todo.Data.Task (SomeTask, someTaskStatusATraversal)
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Data.TaskPriority (TaskPriority)
import Todo.Data.TaskPriority qualified as TaskPriority
import Todo.Data.TaskStatus (TaskStatus, _Blocked, _BlockerId)
import Todo.Data.TaskStatus qualified as TaskStatus
import Todo.Data.Timestamp (Timestamp)
import Todo.Data.Timestamp qualified as Timestamp
import Todo.Exception qualified as E
import Todo.Index (Index, TaskIdNotFoundE (MkTaskIdNotFoundE))
import Todo.Index qualified as Index
import Todo.Prelude
import Todo.Render qualified as Render
import Todo.Utils (MatchResult (MatchFailure, MatchPartial, MatchSuccess))
import Todo.Utils qualified as Utils

setTaskDeadline ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadTerminal m,
    MonadTime m
  ) =>
  CoreConfigMerged ->
  InteractiveSwitch ->
  -- | The task id.
  Maybe TaskId ->
  -- | The task deadline.
  Maybe Timestamp ->
  m ()
setTaskDeadline =
  setTaskValueInteractiveSwitch
    (Timestamp.parseTimestamp . unpack)
    updateDeadline
  where
    updateDeadline taskId newDeadline index = do
      let mSetResult = Index.reallyUnsafeSetTaskValue #deadline taskId (Just newDeadline) index
      liftMatchSuccessM taskId mSetResult

setTaskDescription ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadTerminal m,
    MonadTime m
  ) =>
  CoreConfigMerged ->
  InteractiveSwitch ->
  -- | The task id.
  Maybe TaskId ->
  -- | The task description.
  Maybe Text ->
  m ()
setTaskDescription = setTaskValueInteractiveSwitch EitherRight updateDescription
  where
    updateDescription taskId newDesc index = do
      let mSetResult =
            Index.reallyUnsafeSetTaskValue
              #description
              taskId
              (Just newDesc)
              index
      liftMatchSuccessM taskId mSetResult

setTaskId ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadTerminal m,
    MonadTime m
  ) =>
  CoreConfigMerged ->
  InteractiveSwitch ->
  -- | The current task id.
  Maybe TaskId ->
  -- | The new task id.
  Maybe TaskId ->
  m ()
setTaskId = setTaskValueInteractiveSwitch TaskId.parseTaskId updateId
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
    MonadCatch m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadTerminal m,
    MonadTime m
  ) =>
  CoreConfigMerged ->
  InteractiveSwitch ->
  -- | The task id.
  Maybe TaskId ->
  -- | The task status.
  Maybe TaskPriority ->
  m ()
setTaskPriority =
  setTaskValueInteractiveSwitch
    TaskPriority.parseTaskPriority
    updatePriority
  where
    updatePriority taskId newPriority index = do
      let mSetResult =
            Index.reallyUnsafeSetSomeTaskValue
              #priority
              taskId
              newPriority
              index
      liftJustM taskId mSetResult

setTaskStatus ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadTerminal m,
    MonadTime m
  ) =>
  CoreConfigMerged ->
  InteractiveSwitch ->
  -- | The task id.
  Maybe TaskId ->
  -- | The task status.
  Maybe TaskStatus ->
  m ()
setTaskStatus =
  setTaskValueInteractiveSwitch
    TaskStatus.parseTaskStatus
    updateStatus
  where
    updateStatus taskId newStatus index = do
      mSetResult <- Index.setSomeTaskValueValidate #status taskId newStatus index
      liftJustM taskId mSetResult

setTaskValueInteractiveSwitch ::
  forall m a.
  ( HasCallStack,
    MonadCatch m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadTerminal m,
    MonadTime m
  ) =>
  (Text -> EitherString a) ->
  (TaskId -> a -> Index -> m (Index, SomeTask)) ->
  CoreConfigMerged ->
  InteractiveSwitch ->
  -- | The task id.
  Maybe TaskId ->
  -- | The task value.
  Maybe a ->
  m ()
setTaskValueInteractiveSwitch
  parser
  setIndexFn
  coreConfig
  intMode
  mTaskId
  mNewValue = do
    case intMode of
      InteractiveOn ->
        case (mTaskId, mNewValue) of
          (Just _, _) -> do
            throwString "--task-id is incompatible with --interactive on."
          (_, Just _) ->
            throwString "Value is incompatible with --interactive on."
          (Nothing, Nothing) -> do
            let allIds = L.sort $ Index.getAllIds index

            putTextLn "Found id(s):\n"

            for_ allIds $ \(i, status) -> do
              let idRendered = TaskId.render coreConfig.colorSwitch i
                  statusRendered = TaskStatus.render coreConfig.colorSwitch status
                  pairRendered = idRendered <> ": " <> statusRendered
              putTextLn $ "- " <> builderToTxt pairRendered
            putTextLn ""

            setTaskValueWithRetry parser setIndexFn coreConfig
      InteractiveOff ->
        case (mTaskId, mNewValue) of
          (Nothing, _) -> do
            throwString "--task-id is mandatory with --interactive off."
          (_, Nothing) ->
            throwString "Value is mandatory with --interactive off."
          (Just taskId, Just newValue) ->
            setTaskValue setIndexFn coreConfig taskId newValue
    where
      index = coreConfig.index

setTaskValueWithRetry ::
  forall m a.
  ( HasCallStack,
    MonadCatch m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadTerminal m,
    MonadTime m
  ) =>
  (Text -> EitherString a) ->
  (TaskId -> a -> Index -> m (Index, SomeTask)) ->
  CoreConfigMerged ->
  m ()
setTaskValueWithRetry parser setIndexFn coreConfig = go
  where
    go = do
      eResult <- tryAny $ do
        taskId <- CUtils.askParseQ "Enter task id to update: " TaskId.parseTaskId
        value <- CUtils.askParseQ @_ @a "Enter value: " parser

        (newIndex, modifiedTask) <- setIndexFn taskId value index

        printUpdated coreConfig modifiedTask

        ans <- CUtils.askYesNoQ "Save value (y/n)? "

        if ans
          then
            saveUpdated newIndex
          else
            putTextLn "Taks not updated."

      case eResult of
        Left ex -> do
          putStrLn $ "\n" <> E.displayExceptionSkipKnownCS ex
          putStrLn ""
          go
        Right _ -> pure ()

    index = coreConfig.index

setTaskValue ::
  forall m a.
  ( HasCallStack,
    MonadFileWriter m,
    MonadTerminal m,
    MonadTime m
  ) =>
  (TaskId -> a -> Index -> m (Index, SomeTask)) ->
  CoreConfigMerged ->
  TaskId ->
  -- | The task value.
  a ->
  m ()
setTaskValue setIndexFn coreConfig taskId newValue = do
  (newIndex, modifiedTask) <- setIndexFn taskId newValue index

  printUpdated coreConfig modifiedTask

  saveUpdated newIndex
  where
    index = coreConfig.index

saveUpdated ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadTerminal m
  ) =>
  Index ->
  m ()
saveUpdated newIndex = do
  Index.writeIndex newIndex
  putTextLn "Successfully updated task"

printUpdated ::
  ( HasCallStack,
    MonadTerminal m,
    MonadTime m
  ) =>
  CoreConfigMerged ->
  SomeTask ->
  m ()
printUpdated coreConfig modifiedTask = do
  putTextLn "Updated task:\n"
  rendered <- Render.renderOne color unicode modifiedTask

  putTextLn
    $ TL.toStrict
    $ TLB.toLazyText rendered
  where
    color = coreConfig.colorSwitch
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
