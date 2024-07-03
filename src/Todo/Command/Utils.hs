module Todo.Command.Utils
  ( -- * Get input
    askYesNoQ,
    getStrippedLine,
    getStrippedLineEmpty,

    -- * Buffering
    withNoBuffering,
    noBuffering,
    lineBuffering,
  )
where

import Data.Text qualified as T
import Effects.FileSystem.HandleWriter
  ( BufferMode (LineBuffering, NoBuffering),
    MonadHandleWriter,
  )
import Effects.FileSystem.HandleWriter qualified as HW
import System.IO qualified as IO
import Todo.Prelude

withNoBuffering :: (HasCallStack, MonadHandleWriter m) => m a -> m a
withNoBuffering m = noBuffering *> m <* lineBuffering

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
