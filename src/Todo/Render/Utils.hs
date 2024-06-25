module Todo.Render.Utils
  ( ColorSwitch (..),
    UnicodeSwitch (..),
    colorBuilder,
  )
where

import System.Console.Pretty qualified as Pretty
import Todo.Prelude

data ColorSwitch
  = ColorOn
  | ColorOff
  deriving stock (Eq, Show)

data UnicodeSwitch
  = UnicodeOn
  | UnicodeOff
  deriving stock (Eq, Show)

-- | Helper for coloring text.
colorBuilder :: Pretty.Color -> Text -> Builder
colorBuilder c = displayBuilder . Pretty.color @Text c
