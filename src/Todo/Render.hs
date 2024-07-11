module Todo.Render
  ( -- * High level
    renderOne,
    renderOne',

    -- * Builders
    renderSorted,
    renderSomeTask,
  )
where

import Data.List qualified as L
import Effects.Time (MonadTime (getSystemZonedTime), ZonedTime)
import GHC.Real (fromIntegral)
import Todo.Data.Sorted (SortedTasks)
import Todo.Data.Task
  ( SingleTask (deadline, description, priority, status, taskId),
    SomeTask (SomeTaskGroup, SomeTaskSingle),
    TaskGroup (subtasks, taskId),
  )
import Todo.Data.Task qualified as Task
import Todo.Data.TaskId qualified as TaskId
import Todo.Data.TaskPriority qualified as TaskPriority
import Todo.Data.TaskStatus qualified as TaskStatus
import Todo.Data.Timestamp qualified as Timestamp
import Todo.Prelude
import Todo.Render.Utils
  ( ColorSwitch,
    UnicodeSwitch (UnicodeOff, UnicodeOn),
  )

renderOne ::
  ( HasCallStack,
    MonadTime m
  ) =>
  ColorSwitch ->
  UnicodeSwitch ->
  SomeTask ->
  m Builder
renderOne colorSwitch unicodeSwitch someTask = do
  currTime <- getSystemZonedTime
  let builder = renderOne' currTime colorSwitch unicodeSwitch someTask

  pure builder

renderOne' ::
  ZonedTime ->
  ColorSwitch ->
  UnicodeSwitch ->
  SomeTask ->
  Builder
renderOne' currTime colorSwitch unicodeSwitch =
  renderSomeTask currTime colorSwitch unicodeSwitch 0

-- | Renders a list of sorted tasks.
renderSorted ::
  -- | Current time, for coloring deadlines.
  ZonedTime ->
  -- | Color?
  ColorSwitch ->
  -- | Unicode?
  UnicodeSwitch ->
  -- | Tasks to render.
  SortedTasks ->
  Builder
renderSorted currTime color unicode =
  renderSomeTasks currTime color unicode 0
    . (.unSortedTasks)

renderSomeTasks ::
  (Foldable f, Functor f) =>
  ZonedTime ->
  ColorSwitch ->
  UnicodeSwitch ->
  Word8 ->
  f SomeTask ->
  Builder
renderSomeTasks currTime color unicode nestLvl tasks =
  vsep (renderSomeTask currTime color unicode nestLvl <$> tasks)

renderSomeTask :: ZonedTime -> ColorSwitch -> UnicodeSwitch -> Word8 -> SomeTask -> Builder
renderSomeTask currTime color unicode !nestLvl st = case st of
  SomeTaskSingle t -> renderTask currTime color unicode nestLvl t
  SomeTaskGroup tg -> renderTaskGroup currTime color unicode nestLvl tg

renderTaskGroup ::
  ZonedTime ->
  ColorSwitch ->
  UnicodeSwitch ->
  Word8 ->
  TaskGroup ->
  Builder
renderTaskGroup currTime color unicode nestLvl tg =
  vsepLine
    [ indent nestLvl $ bullet <> "id: " <> TaskId.render color tg.taskId,
        indent nestLvl $ bulletIndent <> "status: " <> TaskStatus.render color (Task.taskGroupStatus tg),
        indent nestLvl $ bulletIndent <> "priority: " <> TaskPriority.render color (Task.taskGroupPriority tg)
    ]
    <> vsep (renderSomeTask currTime color unicode (nestLvl + 1) <$> tg.subtasks)
  where
    (bullet, bulletIndent) = statusToBullet unicode (SomeTaskGroup tg)

renderTask ::
  ZonedTime ->
  ColorSwitch ->
  UnicodeSwitch ->
  Word8 ->
  SingleTask ->
  Builder
renderTask currTime color unicode nestLvl t =
  vsepLine
    [ indent nestLvl $ bullet <> "id: " <> TaskId.render color t.taskId,
      indent nestLvl $ bulletIndent <> "status: " <> TaskStatus.render color t.status,
      indent nestLvl $ bulletIndent <> "priority: " <> TaskPriority.render color t.priority
    ]
    <> mDeadline
    <> mDescription
  where
    mDeadline =
      renderMaybeEmpty
        nestLvl
        (renderTimestamp <$> t.deadline)

    renderTimestamp ts = bulletIndent <> "deadline: " <> Timestamp.render color currTime ts <> line

    mDescription =
      renderMaybeEmpty
        nestLvl
        ((\d -> bulletIndent <> "description: " <> displayBuilder d <> line) <$> t.description)

    (bullet, bulletIndent) = statusToBullet unicode (SomeTaskSingle t)

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

statusToBullet :: UnicodeSwitch -> SomeTask -> Tuple2 Builder Builder
statusToBullet UnicodeOff _ = ("- ", "  ")
statusToBullet UnicodeOn st
  | Task.someTaskIsCompleted st = ("✅ ", "   ")
  | otherwise = ("❌ ", "   ")
