module Todo.Command.Utils
  ( -- * Get input
    askYesNoQ,
    getStrippedLine,
    getStrippedLineEmpty,
  )
where

import Data.Text qualified as T
import Effects.Exception (throwString)
import Effects.Haskeline qualified as Haskeline
import Todo.Prelude

askYesNoQ ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Text ->
  m Bool
askYesNoQ qsn = go
  where
    go = do
      ans <- getStrippedLine qsn

      if
        | ans == "y" -> pure True
        | ans == "n" -> pure False
        | otherwise -> do
            let err =
                  mconcat
                    [ "Bad answer, expected 'y' or 'n', received: '",
                      ans,
                      "'"
                    ]
            putTextLn err
            go

getStrippedLine ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadThrow m
  ) =>
  Text ->
  m Text
getStrippedLine prompt = do
  Haskeline.getInputLine (unpack prompt) >>= \case
    Nothing -> throwString "Input empty"
    Just t -> pure $ T.strip $ pack t

getStrippedLineEmpty ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadThrow m
  ) =>
  Text ->
  m (Maybe Text)
getStrippedLineEmpty prompt =
  getStrippedLine prompt <&> \txt ->
    if T.null txt
      then Nothing
      else Just txt
