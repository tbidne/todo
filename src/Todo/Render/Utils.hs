module Todo.Render.Utils
  ( ColorSwitch (..),
    UnicodeSwitch (..),
    colorBuilder,
  )
where

import System.Console.Pretty qualified as Pretty
import TOML (DecodeTOML (tomlDecoder))
import Todo.Configuration.Default (Default (def))
import Todo.Prelude

data ColorSwitch
  = ColorOn
  | ColorOff
  deriving stock (Eq, Show)

instance Default ColorSwitch where
  def = ColorOn

instance DecodeTOML ColorSwitch where
  tomlDecoder =
    tomlDecoder <&> \case
      True -> ColorOn
      False -> ColorOff

data UnicodeSwitch
  = UnicodeOn
  | UnicodeOff
  deriving stock (Eq, Show)

instance Default UnicodeSwitch where
  def = UnicodeOn

instance DecodeTOML UnicodeSwitch where
  tomlDecoder =
    tomlDecoder <&> \case
      True -> UnicodeOn
      False -> UnicodeOff

-- | Helper for coloring text.
colorBuilder :: Pretty.Color -> Text -> Builder
colorBuilder c = displayBuilder . Pretty.color @Text c
