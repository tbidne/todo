module Todo.Cli.Command.List
  ( listTasks,
  )
where

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Effects.Time (MonadTime (getSystemZonedTime))
import Todo.Cli.Configuration.Core
  ( CoreConfig
      ( colorSwitch,
        index,
        unicodeSwitch
      ),
    CoreConfigMerged,
  )
import Todo.Cli.Prelude
import Todo.Cli.Render qualified as Render
import Todo.Data.Sorted (SortType)
import Todo.Data.Sorted qualified as Sorted
import Todo.Data.Sorted.RevSort (RevSort)
import Todo.Index qualified as Index

-- | Lists tasks from the given file.
listTasks ::
  ( HasCallStack,
    MonadTerminal m,
    MonadTime m
  ) =>
  CoreConfigMerged ->
  -- | The sort type.
  Maybe SortType ->
  -- | Reverses the sort.
  RevSort ->
  m ()
listTasks coreConfig msortType revSort = do
  let xs = snd $ Index.toList index
      sorted = Sorted.sortTasks msortType revSort xs

  currTime <- getSystemZonedTime

  putTextLn
    $ TL.toStrict
    $ TLB.toLazyText
    $ Render.renderSorted currTime color unicode sorted
  where
    color = coreConfig.colorSwitch
    index = coreConfig.index
    unicode = coreConfig.unicodeSwitch
