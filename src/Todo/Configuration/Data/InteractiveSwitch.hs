module Todo.Configuration.Data.InteractiveSwitch
  ( InteractiveSwitch (..),
    interactiveParser,
  )
where

import Options.Applicative (Parser)
import Options.Applicative qualified as OA
import Todo.Configuration.Data.Utils qualified as CDUtils
import Todo.Prelude

data InteractiveSwitch
  = InteractiveOff
  | InteractiveOn
  deriving stock (Eq, Show)

interactiveParser :: InteractiveSwitch -> String -> Parser InteractiveSwitch
interactiveParser defValue helpTxt = do
  p <&> \case
    Just x -> x
    Nothing -> defValue
  where
    p =
      OA.optional
        $ OA.option
          readInteractive
          ( mconcat
              [ OA.short 'i',
                OA.long "interactive",
                OA.metavar "(off | on)",
                CDUtils.mkHelp $ mainHelp ++ helpTxt
              ]
          )
    readInteractive =
      OA.str >>= \case
        "on" -> pure InteractiveOn
        "off" -> pure InteractiveOff
        other -> fail $ "Expected (off | on), received: " <> other

    mainHelp =
      "Interactive mode. If on, specifing other values is an error. "
