{-# LANGUAGE UndecidableInstances #-}

module Todo.Configuration.Data.Utils
  ( parseTextToBoolIso,
    parseBool,
    mkHelp,
  )
where

import Optics.Core (review)
import Options.Applicative qualified as OA
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Todo.Prelude

parseTextToBoolIso ::
  ( LabelOptic "boolIso" An_Iso a a Bool Bool,
    MonadFail m
  ) =>
  Text ->
  m a
parseTextToBoolIso = fmap (review #boolIso) . parseBool

parseBool :: (MonadFail m) => Text -> m Bool
parseBool "on" = pure True
parseBool "off" = pure False
parseBool other = fail $ unpack $ "Expected (off | on), received : " <> other

-- Looks a bit convoluted, but this gets us what we want:
-- 1. lines aligned (paragraph)
-- 2. linebreak at the end (fmap hardline)
mkHelp :: String -> OA.Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph
