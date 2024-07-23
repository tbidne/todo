module Todo.Cli.Configuration.ConfigPhase
  ( -- * Phases
    ConfigPhase (..),

    -- * Type families
    ConfigPhaseF,
    ConfigPhaseMaybeF,
    ConfigPhaseVoidF,
  )
where

import Todo.Cli.Prelude

type ConfigPhaseF :: ConfigPhase -> Type -> Type
type family ConfigPhaseF p a where
  ConfigPhaseF ConfigPhaseArgs a = Maybe a
  ConfigPhaseF ConfigPhaseToml a = Maybe a
  ConfigPhaseF ConfigPhaseMerged a = a

type ConfigPhaseMaybeF :: ConfigPhase -> Type -> Type
type family ConfigPhaseMaybeF p a where
  ConfigPhaseMaybeF ConfigPhaseArgs a = Maybe a
  ConfigPhaseMaybeF ConfigPhaseToml a = Maybe a
  ConfigPhaseMaybeF ConfigPhaseMerged a = Maybe a

type ConfigPhaseVoidF :: ConfigPhase -> Type -> Type
type family ConfigPhaseVoidF p a where
  ConfigPhaseVoidF ConfigPhaseArgs a = Maybe a
  ConfigPhaseVoidF ConfigPhaseToml a = Maybe a
  ConfigPhaseVoidF ConfigPhaseMerged _ = ()

-- | Data "phases" related to configuration.
data ConfigPhase
  = -- | Args phase.
    ConfigPhaseArgs
  | -- | Toml phase.
    ConfigPhaseToml
  | -- | Merged args + toml phase.
    ConfigPhaseMerged
  deriving stock (Eq, Show)
