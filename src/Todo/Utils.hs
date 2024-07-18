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
    neSetTraversal,

    -- ** Preview + Over
    overPreview',
    setPreview',
    overPreviewNode',
    setPreviewNode',
    MatchResult (..),
    overPreviewPartialNode',
    setPreviewPartialNode',

    -- * Aeson
    validateKeys,
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

-- | Like 'over'', except we run the 'set'' on the result of a 'preview'.
-- This is intended for updates that can fail. For example, we want @Index@
-- update to fail if we do not find a task with the desired id. The
-- 'Ixed' class, however, provides an @AffineTraversal' s a@ i.e sets always
-- succeed.
--
-- We therefore generalize to AffineFold and Setter, and run the set on the
-- preview.
overPreview' ::
  forall k is s a.
  ( Is k An_AffineFold,
    Is k A_Setter
  ) =>
  Optic k is s s a a ->
  (a -> a) ->
  s ->
  Maybe s
overPreview' o f s = case preview o s of
  Nothing -> Nothing
  Just a -> Just (set' o (f a) s)
{-# INLINE overPreview' #-}

-- | 'set'' with 'overPreview''.
setPreview' ::
  ( Is k An_AffineFold,
    Is k A_Setter
  ) =>
  Optic k is s s a a ->
  a ->
  s ->
  Maybe s
setPreview' o newA = overPreview' o (const newA)
{-# INLINE setPreview' #-}

-- | 'overPreviewNode'' that sets the field.
setPreviewNode' ::
  AffineTraversal' s a ->
  Lens' a b ->
  b ->
  s ->
  Maybe (Tuple2 s a)
setPreviewNode' o1 o2 b = overPreviewNode' o1 o2 (const b)

-- | Like 'overPreview'', except we also return the updated "inner node",
-- if the update succeeds.
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

-- | Optics matching result.
data MatchResult s a
  = -- | Match completed failed.
    MatchFailure
  | -- | Match "partially" succeeded (context-dependent).
    MatchPartial a
  | -- | Match completely succeeded.
    MatchSuccess s a
  deriving stock (Eq, Show)

-- | Traversal for any Traversable.
traversal :: (Traversable f) => Traversal' (f a) a
traversal = traversalVL traverse

listTraversal :: Traversal' (List a) a
listTraversal = listPredTraversal (const True)

listPredTraversal :: forall a. (a -> Bool) -> Traversal' (List a) a
listPredTraversal pred = traversalVL f
  where
    --
    f :: forall f. (Applicative f) => (a -> f a) -> List a -> f (List a)
    f g = go
      where
        go [] = pure []
        go (x : xs) =
          if pred x
            then (:) <$> g x <*> go xs
            else go xs

-- | Traverses an NESet.
neSetTraversal :: forall a. (Ord a) => Traversal' (NESet a) a
neSetTraversal = traversalVL f
  where
    f :: forall f. (Applicative f) => (a -> f a) -> NESet a -> f (NESet a)
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
