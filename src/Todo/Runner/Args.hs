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
import Todo.Data.Sorted
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Prelude
import Todo.Render.Utils
  ( ColorSwitch (ColorOff, ColorOn),
    UnicodeSwitch (UnicodeOff, UnicodeOn),
  )

-- | Retrieves CLI args.
getArgs :: (HasCallStack, MonadOptparse m) => m Args
getArgs = execParser parserInfoArgs

-- | CLI args.
data Args = MkArgs
  { -- | Command.
    command :: Command,
    -- | Optional coloring.
    colorSwitch :: Maybe ColorSwitch,
    -- | Path to todo json file.
    path :: Maybe OsPath,
    -- | Optional unicode usage.
    unicodeSwitch :: Maybe UnicodeSwitch
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
  colorSwitch <- colorParser
  path <- pathParser
  unicodeSwitch <- unicodeParser
  _ <- version
  _ <- OA.helper
  command <- commandParser
  pure
    $ MkArgs
      { colorSwitch,
        command,
        path,
        unicodeSwitch
      }

version :: Parser (a -> a)
version =
  OA.infoOption
    versNum
    (mconcat [OA.long "version", OA.short 'v', OA.hidden])

versNum :: String
versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)

colorParser :: Parser (Maybe ColorSwitch)
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
        "on" -> pure ColorOn
        "off" -> pure ColorOff
        other -> fail $ "Unrecognized color: " <> unpack other

unicodeParser :: Parser (Maybe UnicodeSwitch)
unicodeParser =
  OA.optional
    $ OA.option
      readUnicode
      ( mconcat
          [ OA.long "unicode",
            OA.metavar "(on | off)",
            mkHelp helpTxt
          ]
      )
  where
    helpTxt = "If enabled, uses unicode in output. Defaults to on."
    readUnicode =
      OA.str >>= \case
        "on" -> pure UnicodeOn
        "off" -> pure UnicodeOff
        other -> fail $ "Unrecognized unicode: " <> unpack other

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
  = CmdDelete TaskId
  | CmdInsert
  | CmdList (Maybe SortType)
  deriving stock (Eq, Show)

commandParser :: Parser Command
commandParser =
  OA.hsubparser
    ( mconcat
        [ mkCommand "delete" deleteParser deleteTxt,
          mkCommand "insert" insertParser insertTxt,
          mkCommand "list" listParser listTxt
        ]
    )
  where
    deleteTxt = mkCmdDesc "Deletes a task."
    insertTxt = mkCmdDesc "Inserts a new task."
    listTxt = mkCmdDesc "Lists the todo index."

    deleteParser = CmdDelete <$> taskIdParser
    insertParser = pure CmdInsert
    listParser = CmdList <$> sortTypeParser

taskIdParser :: Parser TaskId
taskIdParser =
  OA.argument
    (OA.str >>= TaskId.parseTaskId)
    $ mconcat
      [ OA.metavar "TASK_ID",
        mkHelp helpTxt
      ]
  where
    helpTxt = "Task id to delete."

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
