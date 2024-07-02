{- HLINT ignore "Redundant bracket" -}

-- NOTE: HLINT ignore due to OverloadedRecordDot false positives e.g.
-- (some thing).field.

module Todo
  ( -- * Info
    listTasks,

    -- * Update
    deleteTask,
    insertTask,
  )
where

import Data.Bifunctor (Bifunctor (second))
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Effects.FileSystem.HandleWriter
  ( BufferMode (LineBuffering, NoBuffering),
    MonadHandleWriter,
  )
import Effects.FileSystem.HandleWriter qualified as HW
import Effects.Time (MonadTime (getSystemZonedTime))
import Refined (Refined, type (&&))
import Refined qualified as R
import System.IO qualified as IO
import Todo.Data.Sorted (SortType)
import Todo.Data.Sorted qualified as Sorted
import Todo.Data.Task
  ( SomeTask (MultiTask, SingleTask),
    Task
      ( MkTask,
        deadline,
        description,
        priority,
        status,
        taskId
      ),
    TaskGroup (MkTaskGroup, priority, status, subtasks, taskId),
  )
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Data.TaskPriority qualified as TaskPriority
import Todo.Data.TaskStatus qualified as TaskStatus
import Todo.Data.Timestamp qualified as Timestamp
import Todo.Index (Index)
import Todo.Index qualified as Index
import Todo.Index.Safe
  ( GroupIdMember,
    GroupTaskId (MkGroupTaskId),
    IndexWithData (MkIndexWithData, index),
    SingleTaskId (MkSingleTaskId),
    TaskIdNotMember,
  )
import Todo.Index.Safe qualified as Safe
import Todo.Prelude
import Todo.Render qualified as Render
import Todo.Render.Utils (ColorSwitch, UnicodeSwitch)

-- | Inserts new task(s) into the file.
insertTask ::
  forall m.
  ( HasCallStack,
    MonadFail m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadTerminal m,
    MonadTime m,
    MonadThrow m
  ) =>
  OsPath ->
  -- | Is color enabled.
  ColorSwitch ->
  -- | Is unicode enabled.
  UnicodeSwitch ->
  m ()
insertTask tasksPath color unicode = do
  index <- Index.readIndex tasksPath

  noBuffering

  (newIndex, newTaskIds) <- whileApplySetM index getMoreTasksAns mkSomeTask

  lineBuffering

  Index.writeIndex tasksPath newIndex

  currTime <- getSystemZonedTime

  let indexDiff = Index.filterOnIds newTaskIds newIndex
      sorted = Sorted.sortTasks Nothing (Index.toList indexDiff)

  putTextLn "Successfully added task. Modified tasks:\n"
  putTextLn
    $ TL.toStrict
    $ TLB.toLazyText
    $ Render.renderSorted currTime color unicode sorted
  where
    mkSomeTask :: Index -> m (Index, TaskId)
    mkSomeTask index = do
      mParentTaskId <-
        getExtantTaskGroupIdOrEmpty
          "Task id for parent group (leave blank for no parent)? "
          index

      let eIndexGroupId :: Either Index (Refined GroupIdMember (IndexWithData GroupTaskId))
          eIndexGroupId = case mParentTaskId of
            Nothing -> Left index
            Just parentTaskId -> Right parentTaskId

      shouldMkTaskGroup <- askYesNoQ "Create (empty) task group (y/n)? "

      (eIndexWithData, onTaskId) <-
        if shouldMkTaskGroup
          then second (MultiTask .) <$> mkTaskGroup eIndexGroupId
          else second (SingleTask .) <$> mkOneTask eIndexGroupId

      let (newIndex, taskId) = case eIndexWithData of
            Left rIndexNewId ->
              ( Safe.insert (Safe.addTaskToId rIndexNewId onTaskId),
                (R.unrefine rIndexNewId).singleTaskId.unSingleTaskId
              )
            Right rIndexNewIdAndGroupId ->
              ( Safe.insertAtGroupId (Safe.addTaskToIdAndGroupId rIndexNewIdAndGroupId onTaskId),
                (R.unrefine rIndexNewIdAndGroupId).groupTaskId.unGroupTaskId
              )

      pure (newIndex, taskId)

    mkTaskGroup ::
      Either Index (Refined GroupIdMember (IndexWithData GroupTaskId)) ->
      m
        ( Tuple2
            ( Either
                (Refined TaskIdNotMember (IndexWithData SingleTaskId))
                (Refined (GroupIdMember && TaskIdNotMember) (IndexWithData (Tuple2 SingleTaskId GroupTaskId)))
            )
            (TaskId -> TaskGroup)
        )
    mkTaskGroup eIndexGroupId = do
      eTaskId <- case eIndexGroupId of
        Left index -> Left <$> getTaskId "Task group id: " index
        Right rIndexGroupId -> do
          rIndexNewId <- getTaskId "\nTask id: " (R.unrefine rIndexGroupId).index
          -- NOTE: Safe because these two share the same index.
          pure $ Right $ Safe.reallyUnsafeJoinIds rIndexNewId rIndexGroupId

      status <-
        askParseEmptyQ
          "Task status (leave blank for none): "
          TaskStatus.parseTaskStatus

      priority <-
        askParseEmptyQ
          "Task priority (leave blank for none): "
          TaskPriority.parseTaskPriority

      pure
        ( eTaskId,
          \tid ->
            MkTaskGroup
              { taskId = tid,
                priority,
                status,
                subtasks = Empty
              }
        )

    mkOneTask ::
      Either Index (Refined GroupIdMember (IndexWithData GroupTaskId)) ->
      m
        ( Tuple2
            ( Either
                (Refined TaskIdNotMember (IndexWithData SingleTaskId))
                (Refined (GroupIdMember && TaskIdNotMember) (IndexWithData (Tuple2 SingleTaskId GroupTaskId)))
            )
            (TaskId -> Task)
        )
    mkOneTask eIndexGroupId = do
      eTaskId <- case eIndexGroupId of
        Left index -> Left <$> getTaskId "\nTask id: " index
        Right rIndexGroupId -> do
          rIndexNewId <- getTaskId "\nTask id: " (R.unrefine rIndexGroupId).index
          -- NOTE: Safe because these two share the same index.
          pure $ Right $ Safe.reallyUnsafeJoinIds rIndexNewId rIndexGroupId

      status <-
        askParseQ
          "Task status (completed | in-progress | not-started | blocked: <ids>): "
          TaskStatus.parseTaskStatus

      priority <-
        askParseQ
          "Task priority (low | normal | high): "
          TaskPriority.parseTaskPriority

      putText "Description (leave blank for none): "
      description <- getStrippedLineEmpty

      deadline <-
        askParseEmptyQ
          "Deadline (leave blank for none): "
          (Timestamp.parseTimestamp . unpack)

      pure
        ( eTaskId,
          \tid ->
            MkTask
              { deadline,
                description,
                priority,
                status,
                taskId = tid
              }
        )

    getTaskId :: Text -> Index -> m (Refined TaskIdNotMember (IndexWithData SingleTaskId))
    getTaskId qsn index = go
      where
        go = do
          putText qsn
          idTxt <- getStrippedLine
          case TaskId.parseTaskId idTxt of
            EitherLeft err -> do
              putTextLn $ "Bad response: " <> pack err
              go
            EitherRight taskId -> do
              case R.refine (MkIndexWithData index (MkSingleTaskId taskId)) of
                Left ex -> do
                  putTextLn $ displayRefineException' ex
                  go
                Right x -> pure x

    getExtantTaskGroupIdOrEmpty :: Text -> Index -> m (Maybe (Refined GroupIdMember (IndexWithData GroupTaskId)))
    getExtantTaskGroupIdOrEmpty qsn index = go
      where
        go = do
          putText qsn
          mIdTxt <- getStrippedLineEmpty
          case mIdTxt of
            Nothing -> pure Nothing
            Just idTxt ->
              case TaskId.parseTaskId idTxt of
                EitherLeft err -> do
                  putTextLn $ "Bad response: " <> pack err
                  go
                EitherRight taskId -> do
                  case R.refine @GroupIdMember (MkIndexWithData index (MkGroupTaskId taskId)) of
                    Right x -> pure $ Just x
                    Left ex -> do
                      putTextLn $ displayRefineException' ex
                      go

    getMoreTasksAns :: m Bool
    getMoreTasksAns = askYesNoQ "\nCreate a task (y/n)? "

    askParseQ :: Text -> (Text -> EitherString a) -> m a
    askParseQ qsn parser = go
      where
        go = do
          putText qsn

          txt <- getStrippedLine
          case parser txt of
            EitherLeft err -> do
              putTextLn $ "Bad response: " <> pack err
              go
            EitherRight x -> pure x

    askParseEmptyQ :: Text -> (Text -> EitherString a) -> m (Maybe a)
    askParseEmptyQ qsn parser = go
      where
        go = do
          putText qsn

          getStrippedLineEmpty >>= \case
            Nothing -> pure Nothing
            Just txt ->
              case parser txt of
                EitherLeft err -> do
                  putTextLn $ "Bad answer: " <> pack err
                  go
                EitherRight x -> pure $ Just x

-- | Delete a task.
deleteTask ::
  ( HasCallStack,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadTerminal m,
    MonadTime m,
    MonadThrow m
  ) =>
  -- | Path to tasks.json.
  OsPath ->
  -- | Is color enabled.
  ColorSwitch ->
  -- | Is unicode enabled.
  UnicodeSwitch ->
  -- | Task id to delete.
  TaskId ->
  m ()
deleteTask tasksPath color unicode taskId = do
  index <- Index.readIndex tasksPath
  case Index.delete taskId index of
    Left err -> throwM err
    Right (newIndex, st) -> do
      rendered <- Render.renderOne color unicode st

      noBuffering

      putTextLn "Found task:\n"
      putTextLn $ builderToTxt rendered

      ans <- askYesNoQ "Are you sure you want to delete (y/n)? "

      when ans $ do
        Index.writeIndex tasksPath newIndex
        putTextLn "Successfully deleted task"

      lineBuffering

-- | Lists tasks from the given file.
listTasks ::
  ( HasCallStack,
    MonadFileReader m,
    MonadTerminal m,
    MonadThrow m,
    MonadTime m
  ) =>
  -- | Path to tasks.json.
  OsPath ->
  -- | Is color enabled.
  ColorSwitch ->
  -- | Is unicode enabled.
  UnicodeSwitch ->
  -- | The sort type.
  Maybe SortType ->
  m ()
listTasks path color unicode msortType = do
  index <- Index.readIndex path

  let xs = Index.toList index
      sorted = Sorted.sortTasks msortType xs

  currTime <- getSystemZonedTime

  putTextLn
    $ TL.toStrict
    $ TLB.toLazyText
    $ Render.renderSorted currTime color unicode sorted

noBuffering :: (HasCallStack, MonadHandleWriter m) => m ()
noBuffering = setBuffering NoBuffering

lineBuffering :: (HasCallStack, MonadHandleWriter m) => m ()
lineBuffering = setBuffering LineBuffering

setBuffering :: (HasCallStack, MonadHandleWriter m) => BufferMode -> m ()
setBuffering buffMode = setBuff IO.stdin *> setBuff IO.stdout
  where
    setBuff h = HW.hSetBuffering h buffMode

askYesNoQ :: (HasCallStack, MonadTerminal m) => Text -> m Bool
askYesNoQ qsn = go
  where
    go = do
      putText qsn
      ans <- getStrippedLine

      if
        | ans == "y" -> pure True
        | ans == "n" -> pure False
        | otherwise -> do
            putTextLn "Bad answer, expected 'y' or 'n'."
            go

getStrippedLine :: (HasCallStack, MonadTerminal m) => m Text
getStrippedLine = T.strip . pack <$> getLine

getStrippedLineEmpty :: (HasCallStack, MonadTerminal m) => m (Maybe Text)
getStrippedLineEmpty =
  getStrippedLine <&> \txt ->
    if T.null txt
      then Nothing
      else Just txt
