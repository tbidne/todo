module Todo.Data.Task.Render
  ( renderSorted,
  )
where

import Data.List qualified as L
import Effects.Time (ZonedTime)
import GHC.Real (fromIntegral)
import Todo.Data.Task
  ( SomeTask (MultiTask, SingleTask),
    Task (deadline, description, priority, status, taskId),
    TaskGroup (subtasks, taskId),
  )
import Todo.Data.Task qualified as Task
import Todo.Data.Task.Sorted (SortedTasks (unSortedTasks))
import Todo.Data.Task.TaskId qualified as TaskId
import Todo.Data.Task.TaskPriority qualified as TaskPriority
import Todo.Data.Task.TaskStatus qualified as TaskStatus
import Todo.Data.Timestamp qualified as Timestamp
import Todo.Prelude

renderSorted :: ZonedTime -> Bool -> SortedTasks -> Builder
renderSorted currTime b = renderSomeTasks currTime b 0 . unSortedTasks

renderSomeTasks :: (Foldable f, Functor f) => ZonedTime -> Bool -> Word8 -> f SomeTask -> Builder
renderSomeTasks currTime b nestLvl tasks = vsep (renderSomeTask currTime b nestLvl <$> tasks)

renderSomeTask :: ZonedTime -> Bool -> Word8 -> SomeTask -> Builder
renderSomeTask currTime b nestLvl (SingleTask t) = renderTask currTime b nestLvl t
renderSomeTask currTime b nestLvl (MultiTask tg) = renderTaskGroup currTime b nestLvl tg

renderTaskGroup :: ZonedTime -> Bool -> Word8 -> TaskGroup -> Builder
renderTaskGroup currTime b nestLvl tg =
  vsep
    [ indent nestLvl $ "id: " <> TaskId.render b tg.taskId,
      indent nestLvl $ "status: " <> TaskStatus.render b (Task.taskGroupStatus tg),
      indent nestLvl $ "priority: " <> TaskPriority.render b (Task.taskGroupPriority tg),
      line
    ]
    <> vsep (renderSomeTask currTime b (nestLvl + 1) <$> tg.subtasks)

renderTask :: ZonedTime -> Bool -> Word8 -> Task -> Builder
renderTask currTime b nestLvl t =
  vsepLine
    [ indent nestLvl $ "id: " <> TaskId.render b t.taskId,
      indent nestLvl $ "status: " <> TaskStatus.render b t.status,
      indent nestLvl $ "priority: " <> TaskPriority.render b t.priority
    ]
    <> mDeadline
    <> mDescription
  where
    mDeadline =
      renderMaybeEmpty
        nestLvl
        (renderTimestamp <$> t.deadline)

    renderTimestamp ts = "deadline: " <> Timestamp.render b currTime ts <> line
    mDescription =
      renderMaybeEmpty
        nestLvl
        ((\d -> "description: " <> displayBuilder d <> line) <$> t.description)

vsep :: (Foldable f) => f Builder -> Builder
vsep = concatWith (\x y -> x <> line <> y)

vsepLine :: (Foldable f) => f Builder -> Builder
vsepLine = (<> line) . vsep

indent :: Word8 -> Builder -> Builder
indent i b = displayBuilder spc <> b
  where
    spc = L.replicate (fromIntegral i * 2) ' '

line :: Builder
line = "\n"

-- vendored from prettyprinter, for text's Builder
concatWith ::
  (Foldable t) =>
  (Builder -> Builder -> Builder) ->
  t Builder ->
  Builder
concatWith f ds
  | null ds = mempty
  | otherwise = foldr1 f ds

renderMaybeEmpty ::
  Word8 ->
  Maybe Builder ->
  Builder
renderMaybeEmpty _ Nothing = ""
renderMaybeEmpty nestLvl (Just b) = indent nestLvl b
