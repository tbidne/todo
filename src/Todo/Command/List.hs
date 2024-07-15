module Todo.Command.List
  ( listTasks,
  )
where

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Effects.Time (MonadTime (getSystemZonedTime))
import Todo.Configuration.Core
  ( CoreConfig
      ( colorSwitch,
        index,
        unicodeSwitch
      ),
    CoreConfigMerged,
  )
import Todo.Data.Sorted (SortType)
import Todo.Data.Sorted qualified as Sorted
import Todo.Index qualified as Index
import Todo.Prelude
import Todo.Render qualified as Render

-- | Lists tasks from the given file.
listTasks ::
  ( HasCallStack,
    MonadTerminal m,
    MonadTime m
  ) =>
  CoreConfigMerged ->
  -- | The sort type.
  Maybe SortType ->
  m ()
listTasks coreConfig msortType = do
  let xs = snd $ Index.toList index
      sorted = Sorted.sortTasks msortType xs

  currTime <- getSystemZonedTime

  putTextLn
    $ TL.toStrict
    $ TLB.toLazyText
    $ Render.renderSorted currTime color unicode sorted
  where
    color = coreConfig.colorSwitch
    index = coreConfig.index
    unicode = coreConfig.unicodeSwitch
