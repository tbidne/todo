module Todo.Cli.Command.Utils
  ( -- * Get input
    askYesNoQ,
    askParseQ,
    askParseEmptyQ,

    -- * Low level
    getStrippedLine,
    getStrippedLineEmpty,

    -- * Misc
    formatBadResponse,
  )
where

import Data.Text qualified as T
import Effects.Haskeline qualified as Haskeline
import Todo.Cli.Prelude

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

askParseQ ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Text ->
  (Text -> EitherString a) ->
  m a
askParseQ qsn parser = go
  where
    go = do
      txt <- getStrippedLine qsn
      case parser txt of
        EitherLeft err -> do
          putTextLn $ formatBadResponse err
          go
        EitherRight x -> pure x

askParseEmptyQ ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Text ->
  (Text -> EitherString a) ->
  m (Maybe a)
askParseEmptyQ qsn parser = go
  where
    go = do
      getStrippedLineEmpty qsn >>= \case
        Nothing -> pure Nothing
        Just txt ->
          case parser txt of
            EitherLeft err -> do
              putTextLn $ formatBadResponse err
              go
            EitherRight x -> pure $ Just x

getStrippedLine ::
  ( HasCallStack,
    MonadHaskeline m,
    MonadThrow m
  ) =>
  Text ->
  m Text
getStrippedLine prompt = do
  Haskeline.getInputLine (unpack prompt) >>= \case
    Nothing -> throwText "Input empty"
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

formatBadResponse :: String -> Text
formatBadResponse = (\t -> "\nBad Response: " <> t <> "\n") . pack
