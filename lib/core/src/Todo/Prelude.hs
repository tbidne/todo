{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}

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
    foldMappersAltA,

    -- * Collections
    -- ** Lists
    firstJust,
    headMaybe,

    -- ** FromList
    listToSeq,
    listToNESet,
    unsafeListToNonEmpty,

    -- ** ToList
    seqToList,

    -- * Develop
    todo,
    unimpl,
    traceFile,
    traceFileLine,

    -- * Optics
    strTxtIso,
    toListLikeOf,

    -- * Misc
    EitherString (..),
    builderToTxt,
    displayExceptiont,
    stripNulls,
    getTodoXdgConfig,
    mToE,
    throwLeft,
    identity,
    showt,
    setUncaughtExceptionHandlerDisplay,
  )
where

{- ORMOLU_ENABLE -}

import Control.Applicative as X
  ( Alternative (empty, (<|>)),
    Applicative (liftA2, pure, (<*>)),
    asum,
    (*>),
    (<*),
  )
import Control.Exception.Utils as X (throwText, trySync)
import Control.Monad as X
  ( Monad ((>>=)),
    join,
    unless,
    void,
    when,
    (=<<),
    (>=>),
  )
import Control.Monad.Catch as X
  ( Exception (displayException, fromException, toException),
    MonadCatch,
    MonadThrow,
    SomeException (SomeException),
    throwM,
    try,
  )
import Control.Monad.Fail as X (MonadFail (fail))
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Data.Aeson as X (FromJSON (parseJSON), ToJSON (toJSON), Value (Null))
import Data.Aeson.Types (Pair)
import Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import Data.Char as X (Char)
import Data.Coerce as X (coerce)
import Data.Either as X (Either (Left, Right))
import Data.Eq as X (Eq ((==)), (/=))
import Data.Foldable as X
  ( Foldable (fold, foldMap, foldl', foldr, length, null, toList),
    foldr1,
    for_,
  )
import Data.Foldable1 as X (Foldable1 (toNonEmpty))
import Data.Function as X (const, flip, ($), (.))
import Data.Functor as X (Functor (fmap), (<$>), (<&>))
import Data.Int as X (Int)
import Data.Kind as X (Constraint, Type)
#if MIN_VERSION_base(4, 20, 0)
import Data.List as X (List, filter, (++))
#else
import Data.List as X (filter, (++))
#endif
import Data.List.NonEmpty as X (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict as X (Map)
import Data.Maybe as X (Maybe (Just, Nothing), catMaybes, fromMaybe, maybe)
import Data.Maybe.Optics as X ((%?), _Just)
import Data.Monoid as X (Monoid (mconcat, mempty))
import Data.Ord as X
  ( Ord (compare, (<=), (>), (>=)),
    Ordering (EQ, GT, LT),
    max,
    min,
  )
import Data.Semigroup as X (Semigroup (sconcat, (<>)))
import Data.Sequence as X (Seq (Empty, (:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty as X (NESeq ((:<||), (:||>)))
import Data.Set as X (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty as X (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.String as X (IsString (fromString), String)
import Data.Text as X (Text, pack, unpack)
import Data.Text.Builder.Linear as X (Builder)
import Data.Text.Builder.Linear qualified as TBL
import Data.Text.Display as X (Display (displayBuilder), display)
import Data.Traversable as X (Traversable (sequenceA, traverse))
import Data.Tuple as X (curry, snd, uncurry)
#if MIN_VERSION_base(4, 20, 0)
import Data.Tuple.Experimental as X (Tuple2, Tuple3)
#endif
import Data.Tuple.Optics as X (_1, _2)
import Data.Type.Equality as X (type (~))
import Data.Word as X (Word16, Word8)
import Effects.FileSystem.FileReader as X (MonadFileReader)
import Effects.FileSystem.FileWriter as X
  ( MonadFileWriter (writeBinaryFile),
    appendFileUtf8,
  )
import Effects.FileSystem.PathReader as X (MonadPathReader)
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter as X (MonadPathWriter)
import Effects.Time as X (MonadTime)
import FileSystem.OsPath as X (OsPath, encode, osp, ospPathSep, (</>))
import FileSystem.OsPath qualified as OsPath
import FileSystem.UTF8 as X (encodeUtf8)
import GHC.Base (RuntimeRep, TYPE, raise#, seq)
-- import Optics.At.Core as X (ix)

import GHC.Conc.Sync qualified as Sync
import GHC.Enum as X (Bounded, Enum)
import GHC.Err as X (error)
import GHC.Exception (errorCallWithCallStackException)
import GHC.Exts as X (IsList (Item))
import GHC.Exts qualified as Exts
import GHC.IO.Exception (ExitCode (ExitFailure))
import GHC.Num as X (Num ((*), (+), (-)))
import GHC.Records as X (HasField (getField))
import GHC.Show as X (Show (show))
import GHC.Stack as X (HasCallStack)
import Optics.AffineFold as X (An_AffineFold, preview)
import Optics.AffineTraversal as X
  ( AffineTraversal,
    AffineTraversal',
    An_AffineTraversal,
    atraversal,
  )
import Optics.Core as X (Is, Optic, Optic')
import Optics.Core.Extras as X (is)
import Optics.Fold as X (Fold, toListOf)
import Optics.Getter as X (A_Getter, Getter, to, view)
import Optics.Indexed.Core as X (A_Fold, (%))
import Optics.Iso as X (An_Iso, Iso, Iso', iso)
import Optics.Label as X (LabelOptic (labelOptic))
import Optics.Lens as X (A_Lens, Lens, Lens', lens, lensVL)
import Optics.Operators as X ((^.), (^?))
import Optics.Prism as X (Prism, Prism', prism)
import Optics.Re as X (re)
import Optics.Review as X (review)
import Optics.Setter as X (A_Setter, over', set')
import Optics.Traversal as X (A_Traversal, Traversal, Traversal', traversalVL)
import System.Exit as X (ExitCode (ExitSuccess))
import System.IO as X (FilePath, IO)
import System.IO qualified as IO
import System.IO.Unsafe (unsafePerformIO)

#if !MIN_VERSION_base(4, 20, 0)

type List = []

type Tuple2 a b = (a, b)

type Tuple3 a b c = (a, b, c)

#endif

identity :: forall a. a -> a
identity x = x

-- | foldMap for Alternative.
foldMapAlt ::
  forall t f a b.
  ( Alternative f,
    Foldable t
  ) =>
  (a -> f b) ->
  t a ->
  f b
foldMapAlt f = foldr ((<|>) . f) empty
{-# INLINEABLE foldMapAlt #-}

-- | Like 'foldMapAlt', except we fold over mapping functions, rather than
-- the inputs.
foldMappersAlt ::
  forall t f a b.
  ( Alternative f,
    Foldable t
  ) =>
  a ->
  t (a -> f b) ->
  f b
foldMappersAlt x = foldMapAlt (\p -> p x)
{-# INLINEABLE foldMappersAlt #-}

-- | Effectful version of 'foldMappersAlt'.
foldMappersAltA ::
  forall m t f a b.
  ( Alternative f,
    Applicative m,
    Foldable t
  ) =>
  a ->
  t (a -> m (f b)) ->
  m (f b)
foldMappersAltA x = foldr (\g -> liftA2 (<|>) (g x)) (pure empty)
{-# INLINEABLE foldMappersAltA #-}

builderToTxt :: Builder -> Text
builderToTxt = TBL.runBuilder

listToSeq :: List a -> Seq a
listToSeq = Seq.fromList

seqToList :: Seq a -> List a
seqToList = toList

unsafeListToNonEmpty :: (HasCallStack) => List a -> NonEmpty a
unsafeListToNonEmpty = NE.fromList

listToNESet :: (Ord a) => List a -> Maybe (NESet a)
listToNESet = NESet.nonEmptySet . Set.fromList

showt :: (Show a) => a -> Text
showt = pack . show

todo :: forall {r :: RuntimeRep} (a :: TYPE r). (HasCallStack) => a
todo = raise# (errorCallWithCallStackException "Prelude.todo: not yet implemented" ?callStack)
{-# WARNING in "x-todo" todo "todo remains in code" #-}

unimpl :: forall {r :: RuntimeRep} (a :: TYPE r). (HasCallStack) => a
unimpl = raise# (errorCallWithCallStackException "Prelude.unimpl: intentionally implemented" ?callStack)
{-# WARNING in "x-unimpl" unimpl "unimpl remains in code" #-}

traceFile :: FilePath -> Text -> a -> a
traceFile path txt x = writeFn `seq` x
  where
    io = appendFileUtf8 (OsPath.unsafeEncodeValid path) txt
    writeFn = unsafePerformIO io

traceFileLine :: FilePath -> Text -> a -> a
traceFileLine path txt = traceFile path (txt <> "\n")

{- ORMOLU_DISABLE -}

setUncaughtExceptionHandlerDisplay :: IO ()
setUncaughtExceptionHandlerDisplay =
  Sync.setUncaughtExceptionHandler printExceptExitCode
  where
    printExceptExitCode ex = case fromException ex of
      Just ExitSuccess -> pure ()
      -- for command failures
      Just (ExitFailure _) -> pure ()
      Nothing -> IO.putStrLn $ displayException ex

-- | Either, specializing Left to String, for the purposes of MonadFail.
data EitherString a
  = EitherLeft String
  | EitherRight a
  deriving stock (Eq, Functor, Show)

instance Applicative EitherString where
  pure = EitherRight

  EitherRight f <*> EitherRight x = EitherRight (f x)
  EitherLeft x <*> _ = EitherLeft x
  _ <*> EitherLeft x = EitherLeft x

instance Monad EitherString where
  EitherRight x >>= f = f x
  EitherLeft x >>= _ = EitherLeft x

instance Foldable EitherString where
  foldr _ e (EitherLeft _) = e
  foldr f e (EitherRight x) = f x e

instance Traversable EitherString where
  sequenceA (EitherLeft x) = pure (EitherLeft x)
  sequenceA (EitherRight x) = EitherRight <$> x

  traverse _ (EitherLeft x) = pure (EitherLeft x)
  traverse f (EitherRight x) = EitherRight <$> f x

instance MonadFail EitherString where
  fail = EitherLeft

-- | Removes nulls for aeson encoding.
stripNulls :: List Pair -> List Pair
stripNulls = filter (\(_, v) -> v /= Null)

displayExceptiont :: (Exception e) => e -> Text
displayExceptiont = pack . displayException

getTodoXdgConfig :: (HasCallStack, MonadPathReader m) => m OsPath
getTodoXdgConfig = PR.getXdgConfig [osp|todo|]

throwLeft ::
  ( Exception e,
    HasCallStack,
    MonadThrow m
  ) =>
  Either e a ->
  m a
throwLeft (Right x) = pure x
throwLeft (Left err) = throwM err

firstJust :: List (Maybe a) -> Maybe a
firstJust = headMaybe . catMaybes

headMaybe :: List a -> Maybe a
headMaybe (x : _) = Just x
headMaybe [] = Nothing

mToE :: e -> Maybe a -> Either e a
mToE _ (Just x) = Right x
mToE e Nothing = Left e

strTxtIso :: Iso' String Text
strTxtIso = iso pack unpack

toListLikeOf ::
  ( IsList c,
    Is k A_Fold
  ) =>
  Optic' k is a (Item c) ->
  a ->
  c
toListLikeOf optic = Exts.fromList . toListOf optic
