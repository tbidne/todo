module Todo.Command.Delete
  ( deleteTask,
  )
where

import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effects.Time (MonadTime (getSystemZonedTime))
import Todo.Command.Utils qualified as CUtils
import Todo.Configuration.Args
  ( InteractiveSwitch (InteractiveOff, InteractiveOn),
  )
import Todo.Configuration.Core
  ( CoreConfig
      ( colorSwitch,
        index,
        unicodeSwitch
      ),
    CoreConfigMerged,
  )
import Todo.Data.Sorted qualified as Sorted
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Exception qualified as E
import Todo.Index
  ( DeleteE
      ( DeleteRefId,
        DeleteTaskIdNotFound
      ),
    TaskIdNotFoundE (MkTaskIdNotFoundE),
    (∉),
  )
import Todo.Index qualified as Index
import Todo.Prelude
import Todo.Render qualified as Render

-- | Delete a task.
deleteTask ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadTerminal m,
    MonadTime m
  ) =>
  CoreConfigMerged ->
  InteractiveSwitch ->
  Maybe (NESet TaskId) ->
  m ()
deleteTask coreConfig intMode mTaskIds = do
  case intMode of
    InteractiveOn -> do
      when (is _Just mTaskIds)
        $ throwString "--task-id incompatible with --interactive on. Please only specify one."

      let allIds = L.sort $ Index.getAllIds index

      putTextLn "Found id(s):\n"

      for_ allIds $ \i -> do
        let r = builderToTxt $ TaskId.render coreConfig.colorSwitch i
        putTextLn $ "- " <> r
      putTextLn ""

      deleteWithRetry coreConfig
    InteractiveOff -> case mTaskIds of
      Nothing ->
        throwString "When --interactive is off, --task-id is required."
      Just taskIds ->
        deleteIds coreConfig taskIds
  where
    index = coreConfig.index

deleteWithRetry ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadTerminal m,
    MonadTime m
  ) =>
  CoreConfigMerged ->
  m ()
deleteWithRetry coreConfig = go
  where
    go = do
      eResult <- tryAny $ do
        taskIds <- CUtils.askParseQ "Enter task id(s) to delete: " parseTaskIds
        deleteIds coreConfig taskIds

      case eResult of
        Left ex -> do
          putStrLn $ "\n" <> E.displayExceptionSkipKnownCS ex
          putStrLn ""
          go
        Right _ -> pure ()

    parseTaskIds :: Text -> EitherString (NESet TaskId)
    parseTaskIds txt = do
      let txts = T.split (== ' ') txt
      taskIdList <- traverse TaskId.parseTaskId txts
      case listToNESet taskIdList of
        Nothing -> EitherLeft "Did not receive any task ids."
        Just x -> pure x

-- | Delete a task.
deleteIds ::
  ( HasCallStack,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadTerminal m,
    MonadTime m,
    MonadThrow m
  ) =>
  CoreConfigMerged ->
  NESet TaskId ->
  m ()
deleteIds coreConfig taskIds = do
  let (toDelete, toSave) = Index.partitionTaskIds taskIds index
      blockingIds = Index.getBlockingIds toSave

  for_ taskIds $ \taskId -> do
    when
      (taskId ∉ toDelete)
      (throwM $ DeleteTaskIdNotFound $ MkTaskIdNotFoundE taskId)

    case Map.lookup taskId blockingIds of
      Nothing -> pure ()
      Just blocked -> throwM $ DeleteRefId taskId blocked

  currTime <- getSystemZonedTime

  putTextLn "Found task(s):\n"

  let sortedToDelete = sort toDelete
      renderedToDelete = render currTime sortedToDelete

  putTextLn $ builderToTxt renderedToDelete

  ans <- CUtils.askYesNoQ "Are you sure you want to delete (y/n)? "

  if ans
    then do
      Index.writeIndex toSave
      putTextLn "Successfully deleted tasks(s)."
    else
      putTextLn "Did not delete any tasks."
  where
    color = coreConfig.colorSwitch
    index = coreConfig.index
    unicode = coreConfig.unicodeSwitch

    sort = Sorted.sortTasks Nothing False . (.taskList)
    render currTime = Render.renderSorted currTime color unicode
