module Todo.Configuration.Args
  ( Args (..),
    Command (..),
    getArgs,
  )
where

import Data.List qualified as L
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
import Todo.Configuration.ConfigPhase
  ( ConfigPhase (ConfigPhaseArgs),
  )
import Todo.Configuration.Core
  ( CoreConfig (MkCoreConfig, colorSwitch, index, unicodeSwitch),
    IndexConfig (MkIndexConfig, name, path),
  )
import Todo.Data.Sorted
  ( SortType
      ( SortPriority,
        SortPriorityStatus,
        SortStatus,
        SortStatusPriority
      ),
  )
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Data.TaskPriority (TaskPriority)
import Todo.Data.TaskPriority qualified as TaskPriority
import Todo.Data.TaskStatus (TaskStatus)
import Todo.Data.TaskStatus qualified as TaskStatus
import Todo.Data.Timestamp (Timestamp)
import Todo.Data.Timestamp qualified as Timestamp
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
  { -- | Core config.
    coreConfig :: CoreConfig ConfigPhaseArgs,
    -- | Command.
    command :: Command,
    tomlPath :: Maybe OsPath
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
  tomlPath <- configPathParser
  indexName <- indexNameParser
  indexPath <- indexPathParser
  unicodeSwitch <- unicodeParser
  _ <- version
  _ <- OA.helper
  command <- commandParser
  pure
    $ MkArgs
      { coreConfig =
          MkCoreConfig
            { colorSwitch,
              index =
                MkIndexConfig
                  { name = indexName,
                    path = indexPath
                  },
              unicodeSwitch
            },
        command,
        tomlPath
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

configPathParser :: Parser (Maybe OsPath)
configPathParser =
  OA.optional
    $ OA.option
      osPath
      ( mconcat
          [ OA.long "config-path",
            OA.metavar "PATH",
            mkHelp helpTxt
          ]
      )
  where
    helpTxt =
      mconcat
        [ "Path to toml config. If the path is not given, we look in the ",
          "XDG config directory e.g. ~/.config/todo/config.toml."
        ]

indexNameParser :: Parser (Maybe Text)
indexNameParser =
  OA.optional
    $ OA.option
      OA.str
      ( mconcat
          [ OA.long "index-name",
            OA.metavar "STR",
            mkHelp helpTxt
          ]
      )
  where
    helpTxt =
      mconcat
        [ "Name of the index to use. Used in conjunction with the config ",
          "file's index-legend field, to select a legend path from a list ",
          "of paths."
        ]

indexPathParser :: Parser (Maybe OsPath)
indexPathParser =
  OA.optional
    $ OA.option
      osPath
      ( mconcat
          [ OA.long "index-path",
            OA.metavar "PATH",
            mkHelp helpTxt
          ]
      )
  where
    helpTxt = "Path to todo json index. Overrides --index-name."

data Command
  = CmdDelete (NESet TaskId)
  | CmdInsert
  | CmdList (Maybe SortType)
  | CmdSetDeadline TaskId Timestamp
  | CmdSetDescription TaskId Text
  | CmdSetId TaskId TaskId
  | CmdSetPriority TaskId TaskPriority
  | CmdSetStatus TaskId TaskStatus
  deriving stock (Eq, Show)

commandParser :: Parser Command
commandParser =
  OA.hsubparser
    ( mconcat
        [ mkCommand "list" listParser listTxt,
          OA.commandGroup "Information Commands"
        ]
    )
    <|> OA.hsubparser
      ( mconcat
          [ mkCommand "delete" deleteParser deleteTxt,
            mkCommand "insert" insertParser insertTxt,
            OA.commandGroup "Add/Remove Commands"
          ]
      )
    <|> OA.hsubparser
      ( mconcat
          [ mkCommand "set-deadline" setDeadlineParser setDeadlineTxt,
            mkCommand "set-description" setDescParser setDescTxt,
            mkCommand "set-id" setIdParser setIdTxt,
            mkCommand "set-priority" setPriorityParser setPriorityTxt,
            mkCommand "set-status" setStatusParser setStatusTxt,
            OA.commandGroup "Update commands"
          ]
      )
  where
    deleteTxt = mkCmdDesc "Deletes a task."
    insertTxt = mkCmdDesc "Inserts a new task."
    listTxt = mkCmdDesc "Lists the todo index."
    setDeadlineTxt = mkCmdDesc "Updates a single task deadline."
    setDescTxt = mkCmdDesc "Updates a single task description."
    setIdTxt = mkCmdDesc "Updates a task id."
    setStatusTxt = mkCmdDesc "Updates a task status."
    setPriorityTxt = mkCmdDesc "Updates a task priority."

    -- safe because some only returns non-empty.
    deleteParser =
      CmdDelete
        . unsafeListToNESet
        <$> OA.some (taskIdArgParser "TASK_IDs..." "Task id(s) to delete.")
    insertParser = pure CmdInsert
    listParser = CmdList <$> sortTypeParser
    setDeadlineParser =
      CmdSetDeadline
        <$> setTaskIdParser
        <*> taskDeadlineParser
    setDescParser =
      CmdSetDescription
        <$> setTaskIdParser
        <*> taskDescParser
    setIdParser =
      CmdSetId
        <$> setTaskIdParser
        <*> taskIdArgParser "TASK_ID" "New task id."
    setPriorityParser =
      CmdSetPriority
        <$> setTaskIdParser
        <*> taskPriorityParser
    setStatusParser =
      CmdSetStatus
        <$> setTaskIdParser
        <*> taskStatusParser

taskIdArgParser :: String -> String -> Parser TaskId
taskIdArgParser meta help =
  OA.argument
    (OA.str >>= TaskId.parseTaskId)
    $ mconcat
      [ OA.metavar meta,
        mkHelp help
      ]

setTaskIdParser :: Parser TaskId
setTaskIdParser =
  OA.option
    (OA.str >>= TaskId.parseTaskId)
    $ mconcat
      [ OA.long "task-id",
        OA.metavar "TASK_ID",
        mkHelp "Task id to update."
      ]

taskDeadlineParser :: Parser Timestamp
taskDeadlineParser =
  OA.argument
    (OA.str >>= Timestamp.parseTimestamp)
    $ mconcat
      [ OA.metavar Timestamp.metavar,
        mkHelp "Deadline timestamp."
      ]

taskDescParser :: Parser Text
taskDescParser =
  OA.argument
    OA.str
    $ mconcat
      [ OA.metavar "STR",
        mkHelp "Task description."
      ]

taskPriorityParser :: Parser TaskPriority
taskPriorityParser =
  OA.argument
    (OA.str >>= TaskPriority.parseTaskPriority)
    $ mconcat
      [ OA.metavar TaskPriority.metavar,
        mkHelp "Task priority."
      ]

taskStatusParser :: Parser TaskStatus
taskStatusParser =
  OA.argument
    (OA.str >>= TaskStatus.parseTaskStatus)
    $ mconcat
      [ OA.metavar TaskStatus.metavar,
        mkHelp "Task status."
      ]

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
