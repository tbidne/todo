{-# LANGUAGE UndecidableInstances #-}

module Todo.Configuration.Core
  ( -- * Types
    CoreConfig (..),
    IndexConfig (..),

    -- * Misc
    IndexF,
  )
where

import Todo.Configuration.ConfigPhase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    ConfigPhaseF,
  )
import Todo.Index (Index)
import Todo.Prelude
import Todo.Render.Utils (ColorSwitch, UnicodeSwitch)

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
  IndexF ConfigPhaseMerged = Index

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
