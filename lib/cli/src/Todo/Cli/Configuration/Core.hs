{-# LANGUAGE UndecidableInstances #-}

module Todo.Cli.Configuration.Core
  ( -- * Types
    CoreConfig (..),
    IndexConfig (..),

    -- * Synonyms
    CoreConfigArgs,
    CoreConfigToml,
    CoreConfigMerged,

    -- * Misc
    IndexF,
  )
where

import Todo.Cli.Configuration.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseF,
  )
import Todo.Cli.Prelude
import Todo.Cli.Render.Utils (ColorSwitch, UnicodeSwitch)
import Todo.Index (Index𝕍)

data IndexConfig p = MkIndexConfig
  { name :: Maybe Text,
    path :: IndexPathF p
  }

deriving stock instance (Eq (IndexPathF p)) => Eq (IndexConfig p)

deriving stock instance (Show (IndexPathF p)) => Show (IndexConfig p)

type IndexPathF :: ConfigPhase -> Type
type family IndexPathF p where
  IndexPathF ConfigPhaseArgs = Maybe OsPath
  IndexPathF ConfigPhaseToml = ()

type IndexF :: ConfigPhase -> Type
type family IndexF p where
  IndexF ConfigPhaseArgs = IndexConfig ConfigPhaseArgs
  IndexF ConfigPhaseToml = IndexConfig ConfigPhaseToml
  IndexF ConfigPhaseMerged = Index𝕍

type CoreConfig :: ConfigPhase -> Type
data CoreConfig p = MkCoreConfig
  { colorSwitch :: ConfigPhaseF p ColorSwitch,
    index :: IndexF p,
    unicodeSwitch :: ConfigPhaseF p UnicodeSwitch
  }

deriving stock instance
  ( Show (ConfigPhaseF p ColorSwitch),
    Show (IndexF p),
    Show (ConfigPhaseF p UnicodeSwitch)
  ) =>
  Show (CoreConfig p)

deriving stock instance
  ( Eq (ConfigPhaseF p ColorSwitch),
    Eq (IndexF p),
    Eq (ConfigPhaseF p UnicodeSwitch)
  ) =>
  Eq (CoreConfig p)

type CoreConfigArgs = CoreConfig ConfigPhaseArgs

type CoreConfigToml = CoreConfig ConfigPhaseToml

type CoreConfigMerged = CoreConfig ConfigPhaseMerged
