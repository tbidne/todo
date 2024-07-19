{-# LANGUAGE CPP #-}

module Todo.Exception
  ( displayExceptionSkipKnownCS,
  )
where

import Data.Proxy (Proxy (Proxy))
import Effects.Exception (ExceptionProxy (MkExceptionProxy))
import Effects.Exception qualified as Ex
import Todo.Index (DeleteE, TaskIdNotFoundE)
import Todo.Prelude

-- TODO: Probably put more exceptions here, and also move definitions.

displayExceptionSkipKnownCS :: (Exception e) => e -> String
displayExceptionSkipKnownCS = skipKnownExceptions proxies

proxies :: List ExceptionProxy
proxies =
  [ MkExceptionProxy $ Proxy @DeleteE,
    MkExceptionProxy $ Proxy @TaskIdNotFoundE
  ]

skipKnownExceptions :: forall e. (Exception e) => [ExceptionProxy] -> e -> String

{- ORMOLU_DISABLE -}

#if MIN_VERSION_base(4, 20, 0)
skipKnownExceptions = Ex.displayInnerMatch
#else
skipKnownExceptions = Ex.displayCSNoMatch
#endif

{- ORMOLU_ENABLE -}
