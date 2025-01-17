module Todo.Cli.Command.Update
  ( -- * Setters
    setTaskDeadline,
    setTaskDescription,
    setTaskId,
    setTaskPriority,
    setTaskStatus,
  )
where

import Data.List qualified as L
import Todo.Cli.Command.Utils qualified as CUtils
import Todo.Cli.Configuration.Core
  ( CoreConfig (colorSwitch, index, unicodeSwitch),
    CoreConfigMerged,
  )
import Todo.Cli.Configuration.Data.InteractiveSwitch
  ( InteractiveSwitch
      ( InteractiveOff,
        InteractiveOn
      ),
  )
import Todo.Cli.Prelude
import Todo.Cli.Render qualified as Render
import Todo.Cli.Render.TaskId qualified as RTaskId
import Todo.Cli.Render.TaskStatus qualified as RTaskStatus
import Todo.Data.Task (SomeTask)
import Todo.Data.Task.Optics qualified as TaskO
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Data.TaskPriority (TaskPriority)
import Todo.Data.TaskPriority qualified as TaskPriority
import Todo.Data.TaskStatus (TaskStatus, _Blocked, _BlockerId)
import Todo.Data.TaskStatus qualified as TaskStatus
import Todo.Data.Timestamp (Timestamp)
import Todo.Data.Timestamp qualified as Timestamp
import Todo.Exception
  ( FoundGroupNotSingleE (MkFoundGroupNotSingleE),
    TaskIdNotFoundE (MkTaskIdNotFoundE),
  )
import Todo.Exception qualified as E
import Todo.Index (Index𝕌, Index𝕍)
import Todo.Index qualified as Index
import Todo.Index.Optics qualified as IndexO
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
      let mSetResult = Index.setTaskValue #deadline taskId (Just newDeadline) index
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
            Index.setTaskValue
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
      let mSetResult =
            Index.setSomeTaskValueMapped
              updateBlockers
              #taskId
              oldTaskId
              newId
              index
      liftJustM oldTaskId mSetResult
      where
        updateBlockers :: Index𝕌 -> Index𝕌
        updateBlockers = over' indexBlockerIdTraversal g
          where
            -- Targets all blocking ids
            indexBlockerIdTraversal :: Traversal Index𝕌 Index𝕌 TaskId TaskId
            indexBlockerIdTraversal =
              IndexO.indexTraversal
                % TaskO.someTaskStatusATraversal
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
            Index.setSomeTaskValue
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
      let mSetResult = Index.setSomeTaskValue #status taskId newStatus index
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
  (TaskId -> a -> Index𝕍 -> m (Index𝕌, SomeTask)) ->
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
            throwText "--task-id is incompatible with --interactive on."
          (_, Just _) ->
            throwText "Value is incompatible with --interactive on."
          (Nothing, Nothing) -> do
            let allIds = L.sort $ toListOf IndexO.indexIdStatusFold index

            putTextLn "Found id(s):\n"

            for_ allIds $ \(i, status) -> do
              let idRendered = RTaskId.render coreConfig.colorSwitch i
                  statusRendered = RTaskStatus.render coreConfig.colorSwitch status
                  pairRendered = idRendered <> ": " <> statusRendered
              putTextLn $ "- " <> builderToTxt pairRendered
            putTextLn ""

            setTaskValueWithRetry parser setIndexFn coreConfig
      InteractiveOff ->
        case (mTaskId, mNewValue) of
          (Nothing, _) -> do
            throwText "--task-id is mandatory with --interactive off."
          (_, Nothing) ->
            throwText "Value is mandatory with --interactive off."
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
  (TaskId -> a -> Index𝕍 -> m (Index𝕌, SomeTask)) ->
  CoreConfigMerged ->
  m ()
setTaskValueWithRetry parser setIndexFn coreConfig = go
  where
    go = do
      eResult <- trySync $ do
        taskId <- CUtils.askParseQ "Enter task id to update: " TaskId.parseTaskId
        value <- CUtils.askParseQ @_ @a "Enter value: " parser

        (newIndex𝕌, modifiedTask) <- setIndexFn taskId value index

        printUpdated coreConfig modifiedTask

        ans <- CUtils.askYesNoQ "Save value (y/n)? "

        if ans
          then
            saveUpdated newIndex𝕌
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
    MonadThrow m,
    MonadTime m
  ) =>
  (TaskId -> a -> Index𝕍 -> m (Index𝕌, SomeTask)) ->
  CoreConfigMerged ->
  TaskId ->
  -- | The task value.
  a ->
  m ()
setTaskValue setIndexFn coreConfig taskId newValue = do
  (newIndex𝕌, modifiedTask) <- setIndexFn taskId newValue index

  printUpdated coreConfig modifiedTask

  saveUpdated newIndex𝕌
  where
    index = coreConfig.index

saveUpdated ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Index𝕌 ->
  m ()
saveUpdated newIndex𝕌 = do
  newIndex𝕍 <- Index.verify newIndex𝕌
  Index.writeIndex newIndex𝕍
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

  putTextLn $ builderToTxt rendered
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
