module Todo.Utils
  ( -- * Monadic Combinators
    whileM,
    whileApplyM,
    whileApplySetM,
    whileM_,

    -- * Optics
    traversal,
    listTraversal,
    listPredTraversal,
    seqTraversal,
    seqPredTraversal,
    neSetTraversal,

    -- ** Preview + Over
    overPreviewNode',
    setPreviewNode',
    MatchResult (..),
    overPreviewPartialNode',
    setPreviewPartialNode',
    previewPartialNodeOver',

    -- *** Optics
    _MatchSuccess,
    _MatchPartial,
    _MatchFailure,

    -- * Aeson
    validateKeys,

    -- * Misc
    foldableToText,
  )
where

import Data.Aeson (Object)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap (Key)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.Bifunctor (Bifunctor (second))
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as T
import Todo.Prelude

whileM :: (Monad m) => m Bool -> m a -> m (Seq a)
whileM mb mx = go
  where
    go = do
      b <- mb
      if b
        then do
          x <- mx
          (x :<|) <$> go
        else pure Empty
{-# INLINEABLE whileM #-}

whileApplyM ::
  forall m a b.
  (Monad m) =>
  a ->
  m Bool ->
  (a -> m (a, b)) ->
  m (a, Seq b)
whileApplyM initVal mb mx = go initVal
  where
    go :: a -> m (a, Seq b)
    go input = do
      b <- mb
      if b
        then do
          (next, acc) <- mx input
          second (acc :<|) <$> go next
        else pure (input, Empty)
{-# INLINEABLE whileApplyM #-}

whileApplySetM ::
  forall m a b.
  (Monad m, Ord b) =>
  a ->
  m Bool ->
  (a -> m (a, b)) ->
  m (a, Set b)
whileApplySetM initVal mb mx = go initVal
  where
    go :: a -> m (a, Set b)
    go input = do
      b <- mb
      if b
        then do
          (next, acc) <- mx input
          second (Set.insert acc) <$> go next
        else pure (input, Set.empty)
{-# INLINEABLE whileApplySetM #-}

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ mb mx = go
  where
    go = do
      b <- mb
      when b (mx *> go)
{-# INLINEABLE whileM_ #-}

-- | 'overPreviewNode'' that sets the field.
setPreviewNode' ::
  AffineTraversal' s a ->
  Lens' a b ->
  b ->
  s ->
  Maybe (Tuple2 s a)
setPreviewNode' o1 o2 b = overPreviewNode' o1 o2 (const b)

-- | Like 'over'', except we run the 'set'' on the result of a 'preview'.
-- This is intended for updates that can fail. For example, we want @Index@
-- update to fail if we do not find a task with the desired id. The
-- 'Ixed' class, however, provides an @AffineTraversal' s a@ that does not
-- distinguish between a successful update and a match failure. We do here
-- via the returned Maybe.
--
-- Upon success, we also return the updated "inner node".
overPreviewNode' ::
  forall s a b.
  -- | AffineTraversal from a "root type" @s@ to node @a@.
  AffineTraversal' s a ->
  -- | Lens from node @a@ to leaf @l@.
  Lens' a b ->
  -- | Leaf modifier.
  (b -> b) ->
  -- | Root to modify.
  s ->
  -- | The new root @s'@ and inner node @a'@, if the update was successful.
  Maybe (Tuple2 s a)
overPreviewNode' rootToNode nodeToLeaf f root = case preview rootToNode root of
  Nothing -> Nothing
  Just node ->
    let leaf = view nodeToLeaf node
        leaf' = f leaf
        node' = set' nodeToLeaf leaf' node
     in Just (set' rootToNode node' root, node')
{-# INLINE overPreviewNode' #-}

setPreviewPartialNode' ::
  AffineTraversal' s a ->
  AffineTraversal' a b ->
  b ->
  s ->
  MatchResult s a
setPreviewPartialNode' rootToNode nodeToLeaf x =
  overPreviewPartialNode' rootToNode nodeToLeaf (const x)

-- | Like 'overPreviewNode'', except the inner optic is an AffineTraversal,
-- not a Lens. The overall AffineTraversal is split so that we can
-- distinguish between a complete failure ('MatchFailure') and a partial
-- failures ('MatchParial').
--
-- This is used, for instance, to distinguish between:
--
--   - Failing to find a Task given a TaskId.
--   - Finding a TaskGroup with the TaskId when we wanted a SingleTask.
--
-- for the purposes of giving a better error message.
overPreviewPartialNode' ::
  forall s a b.
  -- | AffineTraversal from a "root type" @s@ to node @a@.
  AffineTraversal' s a ->
  -- | AffineTraversal from node @a@ to leaf @l@.
  AffineTraversal' a b ->
  -- | Leaf modifier.
  (b -> b) ->
  -- | Root to modify.
  s ->
  -- | The new root @s'@ and inner node @a'@, if the update was successful.
  MatchResult s a
overPreviewPartialNode' rootToNode nodeToLeaf f root = case preview rootToNode root of
  Nothing -> MatchFailure
  Just node -> case preview nodeToLeaf node of
    Nothing -> MatchPartial node
    Just leaf ->
      let leaf' = f leaf
          node' = set' nodeToLeaf leaf' node
       in MatchSuccess (set' rootToNode node' root) node'
{-# INLINE overPreviewPartialNode' #-}

-- | Like 'overPreviewPartialNode', except instead of returning the updated
-- type and inner node, we return a function for modifying the type.
-- This is useful when want to perform a match first but only carry out the
-- actual update after doing some intermediate work.
previewPartialNodeOver' ::
  -- | AffineTraversal from a "root type" @s@ to node @a@.
  AffineTraversal' s a ->
  -- | AffineTraversal from node @a@ to leaf @l@.
  AffineTraversal' a b ->
  s ->
  -- | Functions @f@ and @g@ for modifying the outer and inner nodes,
  -- respectively. As @f@ also runs @g@ itself, the latter function is only
  -- useful for examining the possible partial.
  MatchResult ((b -> b) -> s) ((b -> b) -> a)
previewPartialNodeOver' rootToNode nodeToLeaf root = case preview rootToNode root of
  Nothing -> MatchFailure
  Just node -> case preview nodeToLeaf node of
    Nothing -> MatchPartial (const node)
    Just leaf ->
      let h f = set' nodeToLeaf (f leaf) node
          g f =
            let node' = h f
             in set' rootToNode node' root
       in MatchSuccess g h
{-# INLINE previewPartialNodeOver' #-}

-- | Optics matching result.
data MatchResult s a
  = -- | Match completed failed.
    MatchFailure
  | -- | Match "partially" succeeded (context-dependent).
    MatchPartial a
  | -- | Match completely succeeded.
    MatchSuccess s a
  deriving stock (Eq, Show)

_MatchFailure :: Prism' (MatchResult s a) ()
_MatchFailure =
  prism
    (const MatchFailure)
    ( \case
        MatchSuccess s a -> Left $ MatchSuccess s a
        MatchPartial x -> Left $ MatchPartial x
        MatchFailure -> Right ()
    )
{-# INLINE _MatchFailure #-}

_MatchPartial :: Prism' (MatchResult s a) a
_MatchPartial =
  prism
    MatchPartial
    ( \case
        MatchSuccess s a -> Left $ MatchSuccess s a
        MatchPartial x -> Left $ MatchPartial x
        MatchFailure -> Left MatchFailure
    )
{-# INLINE _MatchPartial #-}

_MatchSuccess :: Prism (MatchResult s a) (MatchResult t a) (Tuple2 s a) (Tuple2 t a)
_MatchSuccess =
  prism
    (uncurry MatchSuccess)
    ( \case
        MatchSuccess s a -> Right (s, a)
        MatchPartial x -> Left $ MatchPartial x
        MatchFailure -> Left MatchFailure
    )
{-# INLINE _MatchSuccess #-}

-- | Traversal for any Traversable.
traversal :: (Traversable f) => Traversal (f a) (f b) a b
traversal = traversalVL traverse

listTraversal :: Traversal (List a) (List b) a b
listTraversal = listPredTraversal (const True)

listPredTraversal :: forall a b. (a -> Bool) -> Traversal (List a) (List b) a b
listPredTraversal pred = traversalVL f
  where
    --
    f :: forall f. (Applicative f) => (a -> f b) -> List a -> f (List b)
    f g = go
      where
        go [] = pure []
        go (x : xs) =
          if pred x
            then (:) <$> g x <*> go xs
            else go xs

seqTraversal :: Traversal (Seq a) (Seq b) a b
seqTraversal = seqPredTraversal (const True)

seqPredTraversal :: forall a b. (a -> Bool) -> Traversal (Seq a) (Seq b) a b
seqPredTraversal pred = traversalVL f
  where
    --
    f :: forall f. (Applicative f) => (a -> f b) -> Seq a -> f (Seq b)
    f g = go
      where
        go Empty = pure Empty
        go (x :<| xs) =
          if pred x
            then (:<|) <$> g x <*> go xs
            else go xs

-- | Traverses an NESet.
neSetTraversal :: forall a b. (Ord b) => Traversal (NESet a) (NESet b) a b
neSetTraversal = traversalVL f
  where
    f :: forall f. (Applicative f) => (a -> f b) -> NESet a -> f (NESet b)
    f g =
      fmap NESet.fromList
        . traverse g
        . NESet.toList

-- | Validates that the keys in the object are only those we expect i.e.
-- fails if the Object contains unexpected keys.
validateKeys :: Set Key -> Object -> Parser ()
validateKeys expectedKeys o = do
  case filter (`Set.notMember` expectedKeys) (KM.keys o) of
    [] -> pure ()
    ks@(_ : _) ->
      fail
        $ mconcat
          [ "Found unexpected json key(s): ",
            unpack $ displayKeys ks
          ]
  where
    displayKeys ks =
      T.intercalate
        ", "
        (quote . K.toText <$> ks)

    quote k = "'" <> k <> "'"

foldableToText ::
  (Foldable f) =>
  (a -> Text) ->
  f a ->
  Text
foldableToText onBlocker =
  T.intercalate ", "
    . fmap onBlocker
    . toList
