module Todo.Runner.Args
  ( Args (..),
    Command (..),
    getArgs,
  )
where

import Data.List qualified as L
import Data.String (IsString (fromString))
import Data.Version (Version (versionBranch))
import Effects.Optparse (MonadOptparse (execParser), osPath)
import Options.Applicative
  ( CommandFields,
    InfoMod,
    Mod,
    Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Paths_todo qualified as Paths
import Todo.Data.Task.Sorted
import Todo.Prelude

-- | Retrieves CLI args.
getArgs :: (HasCallStack, MonadOptparse m) => m Args
getArgs = execParser parserInfoArgs

-- | CLI args.
data Args = MkArgs
  { -- | Command.
    command :: Command,
    -- | Path to todo json file.
    path :: Maybe OsPath
  }
  deriving stock (Eq, Show)

parserInfoArgs :: ParserInfo Args
parserInfoArgs =
  ParserInfo
    { infoParser = argsParser,
      infoFullDesc = True,
      infoProgDesc = desc,
      infoHeader = Chunk headerTxt,
      infoFooter = Chunk footerTxt,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    headerTxt = Just "Todo: A simple todo app."
    footerTxt = Just $ fromString versNum
    desc =
      Chunk.paragraph
        "Todo is an application for managing a todo list."

argsParser :: Parser Args
argsParser = do
  path <- pathParser
  _ <- version
  _ <- OA.helper
  command <- commandParser
  pure
    $ MkArgs
      { command,
        path
      }

version :: Parser (a -> a)
version =
  OA.infoOption
    versNum
    (mconcat [OA.long "version", OA.short 'v', OA.hidden])

versNum :: String
versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)

colorParser :: Parser (Maybe Bool)
colorParser =
  OA.optional
    $ OA.option
      readColor
      ( mconcat
          [ OA.long "color",
            OA.metavar "(on | off)",
            mkHelp helpTxt
          ]
      )
  where
    helpTxt = "If enabled, colors output. Defaults to on."
    readColor =
      OA.str >>= \case
        "on" -> pure True
        "off" -> pure False
        other -> fail $ "Unrecognized color: " <> unpack other

pathParser :: Parser (Maybe OsPath)
pathParser =
  OA.optional
    $ OA.option
      osPath
      ( mconcat
          [ OA.long "path",
            OA.metavar "(none | PATH)",
            mkHelp helpTxt
          ]
      )
  where
    helpTxt =
      mconcat
        [ "Path to todo json index. If the path is not given, we look in the ",
          "XDG config directory e.g. ~/.config/todo/todo.config."
        ]

data Command
  = CmdList (Maybe Bool) (Maybe SortType)
  deriving stock (Eq, Show)

commandParser :: Parser Command
commandParser =
  OA.hsubparser
    ( mconcat
        [ mkCommand "list" delParser delTxt
        ]
    )
  where
    delTxt = mkCmdDesc "Lists the todo index."

    delParser =
      CmdList
        <$> colorParser
        <*> sortTypeParser

sortTypeParser :: Parser (Maybe SortType)
sortTypeParser =
  OA.optional
    $ OA.option
      readSortType
    $ mconcat
      [ OA.long "sort",
        OA.metavar "(priority | status | priority_status | status_priority)",
        mkHelp helpTxt
      ]
  where
    readSortType =
      OA.str >>= \case
        "priority" -> pure SortPriority
        "status" -> pure SortStatus
        "priority_status" -> pure SortPriorityStatus
        "status_priority" -> pure SortStatusPriority
        other -> fail $ "Unexpected sort: " <> unpack other

    helpTxt = "How to sort the results."

mkCommand :: String -> Parser a -> InfoMod a -> Mod CommandFields a
mkCommand cmdTxt parser helpTxt = OA.command cmdTxt (OA.info parser helpTxt)

-- Looks a bit convoluted, but this gets us what we want:
-- 1. lines aligned (paragraph)
-- 2. linebreak at the end (fmap hardline)
mkHelp :: String -> OA.Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph

mkCmdDesc :: String -> InfoMod a
mkCmdDesc =
  OA.progDescDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph
