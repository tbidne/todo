module Todo
  ( listTasks,
  )
where

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Effects.Time (MonadTime (getSystemZonedTime))
import Todo.Data.Task.Render qualified as Render
import Todo.Data.Task.Sorted (SortType)
import Todo.Data.Task.Sorted qualified as Sorted
import Todo.Index qualified as Index
import Todo.Prelude

listTasks ::
  ( HasCallStack,
    MonadFileReader m,
    MonadTerminal m,
    MonadTime m,
    MonadThrow m
  ) =>
  -- | Path to todo.json.
  OsPath ->
  -- | Is color enabled.
  Bool ->
  -- | The sort type.
  Maybe SortType ->
  m ()
listTasks path color msortType = do
  index <- Index.readIndex path

  let xs = Index.toList index
      sorted = Sorted.sortTasks msortType xs

  currTime <- getSystemZonedTime

  putTextLn
    $ TL.toStrict
    $ TLB.toLazyText
    $ Render.renderSorted currTime color sorted
