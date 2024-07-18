module Todo.Command.Delete
  ( deleteTask,
  )
where

import Data.Map.Strict qualified as Map
import Effects.Time (MonadTime (getSystemZonedTime))
import Todo.Command.Utils qualified as CUtils
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
deleteTask coreConfig taskIds = do
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
