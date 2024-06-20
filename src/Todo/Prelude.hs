{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

{- ORMOLU_DISABLE -}

module Todo.Prelude
  ( module X,

#if !MIN_VERSION_base(4, 20, 0)

    -- * Anti-punning aliases
    List,
    Tuple2,
    Tuple3,

#endif

    -- * Folding
    foldMapAlt,
    foldMappersAlt,

    -- * From List
    listToSeq,
    unsafeListToNonEmpty,
    unsafeListToNESeq,

    -- * Develop
    todo,
    traceFile,
    traceFileLine,

    -- * Misc
    identity,
    showt,
    setUncaughtExceptionHandlerDisplay,
  )
where

{- ORMOLU_ENABLE -}

import Control.Applicative as X
  ( Alternative (empty, (<|>)),
    Applicative (pure, (<*>)),
    asum,
    (*>),
    (<*),
  )
import Control.Monad as X (Monad ((>>=)), join, void, when, (>=>))
import Control.Monad.Fail as X (MonadFail (fail))
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Data.Aeson as X (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import Data.Either as X (Either (Left, Right))
import Data.Eq as X (Eq ((==)), (/=))
import Data.Foldable as X
  ( Foldable (foldMap, foldl', foldr, length, null, toList),
    foldr1,
    for_,
  )
import Data.Foldable1 as X (Foldable1 (toNonEmpty))
import Data.Function as X (flip, ($), (.))
import Data.Functor as X (Functor (fmap), (<$>))
import Data.Int as X (Int)
import Data.Kind as X (Constraint, Type)
#if MIN_VERSION_base(4, 20, 0)
import Data.List as X (List, filter)
#else
import Data.List as X (filter)
#endif
import Data.List.NonEmpty as X (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict as X (Map)
import Data.Maybe as X (Maybe (Just, Nothing), fromMaybe)
import Data.Monoid as X (Monoid (mconcat, mempty))
import Data.Ord as X (Ord (compare, (<=)), Ordering (EQ, GT, LT), max, min)
import Data.Semigroup as X (Semigroup (sconcat, (<>)))
import Data.Sequence as X (Seq (Empty, (:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty as X (NESeq ((:<||), (:||>)))
import Data.Sequence.NonEmpty qualified as NESeq
import Data.String as X (String)
import Data.Text as X (Text, pack, unpack)
import Data.Text.Display as X (Display (displayBuilder), display)
import Data.Text.Lazy.Builder as X (Builder)
import Data.Traversable as X (Traversable (traverse))
import Data.Tuple as X (snd)
#if MIN_VERSION_base(4, 20, 0)
import Data.Tuple.Experimental as X (Tuple2, Tuple3)
#endif
import Data.Word as X (Word16, Word8)
import Effects.Exception as X
  ( Exception (displayException, fromException),
    ExitCode (ExitSuccess),
    MonadThrow,
    throwM,
  )
import Effects.Exception qualified as Ex
import Effects.FileSystem.FileReader as X (MonadFileReader)
import Effects.FileSystem.FileWriter (appendFileUtf8)
import Effects.FileSystem.PathReader as X (MonadPathReader)
import Effects.FileSystem.Utils as X (OsPath, encodeUtf8, osp, (</>))
import Effects.FileSystem.Utils qualified as FsUtils
import Effects.Optparse as X (MonadOptparse)
import Effects.System.Terminal as X (MonadTerminal (putStrLn), putTextLn)
import Effects.Time as X (MonadTime)
import GHC.Base (RuntimeRep, TYPE, raise#, seq)
import GHC.Enum as X (Bounded, Enum)
import GHC.Err as X (error)
import GHC.Exception (errorCallWithCallStackException)
import GHC.IO.Exception (ExitCode (ExitFailure))
import GHC.Num as X (Num ((*), (+)))
import GHC.Show as X (Show (show))
import GHC.Stack as X (HasCallStack)
import System.IO as X (FilePath, IO)
import System.IO.Unsafe (unsafePerformIO)

#if !MIN_VERSION_base(4, 20, 0)

type List = []

type Tuple2 a b = (a, b)

type Tuple3 a b c = (a, b, c)

#endif

identity :: forall a. a -> a
identity x = x

foldMapAlt :: forall f t a b. (Alternative f, Foldable t) => (a -> f b) -> t a -> f b
foldMapAlt f = foldr ((<|>) . f) empty
{-# INLINEABLE foldMapAlt #-}

foldMappersAlt :: forall f t a b. (Alternative f, Foldable t) => a -> t (a -> f b) -> f b
foldMappersAlt x = foldMapAlt (\p -> p x)
{-# INLINEABLE foldMappersAlt #-}

listToSeq :: List a -> Seq a
listToSeq = Seq.fromList

unsafeListToNonEmpty :: (HasCallStack) => List a -> NonEmpty a
unsafeListToNonEmpty = NE.fromList

unsafeListToNESeq :: (HasCallStack) => List a -> NESeq a
unsafeListToNESeq = NESeq.fromList . unsafeListToNonEmpty

showt :: (Show a) => a -> Text
showt = pack . show

todo :: forall {r :: RuntimeRep} (a :: TYPE r). (HasCallStack) => a
todo = raise# (errorCallWithCallStackException "Prelude.todo: not yet implemented" ?callStack)
{-# WARNING in "x-todo" todo "todo remains in code" #-}

traceFile :: FilePath -> Text -> a -> a
traceFile path txt x = writeFn `seq` x
  where
    io = appendFileUtf8 (FsUtils.unsafeEncodeFpToOs path) txt
    writeFn = unsafePerformIO io

traceFileLine :: FilePath -> Text -> a -> a
traceFileLine path txt = traceFile path (txt <> "\n")

{- ORMOLU_DISABLE -}

-- | TODO: Remove branch once unconditionally on GHC 9.10+.
setUncaughtExceptionHandlerDisplay :: IO ()
setUncaughtExceptionHandlerDisplay =
  Ex.setUncaughtExceptionHandler printExceptExitCode
  where
#if MIN_VERSION_base(4, 20, 0)
    printExceptExitCode ex = case fromException ex of
      Just ExitSuccess -> pure ()
      -- for command failures
      Just (ExitFailure _) -> pure ()
      Nothing -> putStrLn $ displayException ex
#else
    printExceptExitCode ex = case fromException ex of
      Just (Ex.MkExceptionCS ExitSuccess _) -> pure ()
      -- for command failures
      Just (Ex.MkExceptionCS (ExitFailure _) _) -> pure ()
      Nothing -> putStrLn $ displayException ex
#endif

{- ORMOLU_ENABLE -}
