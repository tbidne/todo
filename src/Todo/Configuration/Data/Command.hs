{-# LANGUAGE UndecidableInstances #-}

module Todo.Configuration.Data.Command
  ( -- * Command Type
    Command (..),
    CommandArgs,
    CommandMerged,
    commandParser,
    advancePhase,
  )
where

import Options.Applicative
  ( CommandFields,
    InfoMod,
    Mod,
    Parser,
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Todo.Configuration.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseMerged
      ),
    ConfigPhaseF,
  )
import Todo.Configuration.Data.InteractiveSwitch
  ( InteractiveSwitch (InteractiveOn),
  )
import Todo.Configuration.Data.InteractiveSwitch qualified as InteractiveSwitch
import Todo.Configuration.Data.RevSort (RevSort)
import Todo.Configuration.Data.Utils qualified as CDUtils
import Todo.Configuration.Default qualified as D
import Todo.Data.Sorted (SortType)
import Todo.Data.Sorted qualified as SortType
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Data.TaskPriority (TaskPriority)
import Todo.Data.TaskPriority qualified as TaskPriority
import Todo.Data.TaskStatus (TaskStatus)
import Todo.Data.TaskStatus qualified as TaskStatus
import Todo.Data.Timestamp (Timestamp)
import Todo.Data.Timestamp qualified as Timestamp
import Todo.Prelude

type Command :: ConfigPhase -> Type
data Command p
  = CmdDelete InteractiveSwitch (Maybe (NESet TaskId))
  | CmdInsert
  | CmdList (Maybe SortType) (ConfigPhaseF p RevSort)
  | CmdSetDeadline TaskId Timestamp
  | CmdSetDescription TaskId Text
  | CmdSetId TaskId TaskId
  | CmdSetPriority TaskId TaskPriority
  | CmdSetStatus TaskId TaskStatus

deriving stock instance (Eq (ConfigPhaseF p RevSort)) => Eq (Command p)

deriving stock instance (Show (ConfigPhaseF p RevSort)) => Show (Command p)

type CommandArgs = Command ConfigPhaseArgs

type CommandMerged = Command ConfigPhaseMerged

commandParser :: Parser (Command ConfigPhaseArgs)
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
      (\intMode taskIds -> CmdDelete intMode (listToNESet taskIds))
        <$> interactiveDefOnParser
          "Defaults to on. If on, --task-id is an error."
        <*> OA.many
          ( taskIdArgParser
              "TASK_IDs..."
              "Task id(s) to delete. Only available with --interactive off."
          )
    insertParser = pure CmdInsert
    listParser =
      CmdList
        <$> sortTypeParser
        <*> revSortParser
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

interactiveDefOnParser :: String -> Parser InteractiveSwitch
interactiveDefOnParser = InteractiveSwitch.interactiveParser InteractiveOn

revSortParser :: Parser (Maybe RevSort)
revSortParser =
  OA.optional
    $ OA.option
      (OA.str >>= CDUtils.parseTextToBoolIso)
    $ mconcat
      [ OA.long "reverse",
        OA.metavar "(off | on)",
        CDUtils.mkHelp "Reverses the sort."
      ]

taskIdArgParser :: String -> String -> Parser TaskId
taskIdArgParser meta help =
  OA.argument
    (OA.str >>= TaskId.parseTaskId)
    $ mconcat
      [ OA.metavar meta,
        CDUtils.mkHelp help
      ]

setTaskIdParser :: Parser TaskId
setTaskIdParser =
  OA.option
    (OA.str >>= TaskId.parseTaskId)
    $ mconcat
      [ OA.long "task-id",
        OA.metavar "TASK_ID",
        CDUtils.mkHelp "Task id to update."
      ]

taskDeadlineParser :: Parser Timestamp
taskDeadlineParser =
  OA.argument
    (OA.str >>= Timestamp.parseTimestamp)
    $ mconcat
      [ OA.metavar Timestamp.metavar,
        CDUtils.mkHelp "Deadline timestamp."
      ]

taskDescParser :: Parser Text
taskDescParser =
  OA.argument
    OA.str
    $ mconcat
      [ OA.metavar "STR",
        CDUtils.mkHelp "Task description."
      ]

taskPriorityParser :: Parser TaskPriority
taskPriorityParser =
  OA.argument
    (OA.str >>= TaskPriority.parseTaskPriority)
    $ mconcat
      [ OA.metavar TaskPriority.metavar,
        CDUtils.mkHelp "Task priority."
      ]

taskStatusParser :: Parser TaskStatus
taskStatusParser =
  OA.argument
    (OA.str >>= TaskStatus.parseTaskStatus)
    $ mconcat
      [ OA.metavar TaskStatus.metavar,
        CDUtils.mkHelp "Task status."
      ]

sortTypeParser :: Parser (Maybe SortType)
sortTypeParser =
  OA.optional
    $ OA.option
      (SortType.parseSortType OA.str)
    $ mconcat
      [ OA.long "sort",
        OA.metavar "(priority | status | priority_status | status_priority)",
        CDUtils.mkHelp helpTxt
      ]
  where
    helpTxt = "How to sort the results."

mkCommand :: String -> Parser a -> InfoMod a -> Mod CommandFields a
mkCommand cmdTxt parser helpTxt = OA.command cmdTxt (OA.info parser helpTxt)

mkCmdDesc :: String -> InfoMod a
mkCmdDesc =
  OA.progDescDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph

advancePhase :: CommandArgs -> CommandMerged
advancePhase (CmdDelete a b) = CmdDelete a b
advancePhase CmdInsert = CmdInsert
advancePhase (CmdList a b) = CmdList a (D.fromDefault b)
advancePhase (CmdSetDeadline a b) = CmdSetDeadline a b
advancePhase (CmdSetDescription a b) = CmdSetDescription a b
advancePhase (CmdSetId a b) = CmdSetId a b
advancePhase (CmdSetPriority a b) = CmdSetPriority a b
advancePhase (CmdSetStatus a b) = CmdSetStatus a b
