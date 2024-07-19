module Todo.Configuration.Args
  ( Args (..),
    getArgs,
  )
where

import Data.List qualified as L
import Data.Version (Version (versionBranch))
import Effects.Optparse (MonadOptparse (execParser), osPath)
import Options.Applicative
  ( Parser,
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
import Options.Applicative.Types (ArgPolicy (Intersperse))
import Paths_todo qualified as Paths
import Todo.Configuration.ConfigPhase
  ( ConfigPhase (ConfigPhaseArgs),
  )
import Todo.Configuration.Core
  ( CoreConfig (MkCoreConfig, colorSwitch, index, unicodeSwitch),
    IndexConfig (MkIndexConfig, name, path),
  )
import Todo.Configuration.Data.Command (CommandArgs)
import Todo.Configuration.Data.Command qualified as Command
import Todo.Configuration.Data.Utils qualified as CDUtils
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
    command :: CommandArgs,
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
  command <- Command.commandParser
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
            CDUtils.mkHelp helpTxt
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
            CDUtils.mkHelp helpTxt
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
            CDUtils.mkHelp helpTxt
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
            CDUtils.mkHelp helpTxt
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
            CDUtils.mkHelp helpTxt
          ]
      )
  where
    helpTxt = "Path to todo json index. Overrides --index-name."
