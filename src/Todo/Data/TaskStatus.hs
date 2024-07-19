{-# LANGUAGE ViewPatterns #-}

module Todo.Data.TaskStatus
  ( -- * Status
    TaskStatus (..),
    parseTaskStatus,
    filterBlockingIds,
    isCompleted,
    render,
    metavar,

    -- ** Optics
    _Completed,
    _NotStarted,
    _InProgress,
    _Blocked,

    -- * Blocker
    Blocker (..),
    parseBlockedTarget,

    -- ** Optics
    _BlockerId,
    _BlockerText,
  )
where

import Data.Aeson qualified as Asn
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as T
import System.Console.Pretty qualified as Pretty
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Prelude
import Todo.Render.Utils (ColorSwitch (ColorOff, ColorOn))
import Todo.Render.Utils qualified as Render.Utils

-- | Something that blocks another task.
data Blocker
  = -- | A task that blocks another task. The task is is the blocker.
    BlockerId TaskId
  | -- | General description that blocks a task.
    BlockerText Text
  deriving stock (Eq, Ord, Show)

instance ToJSON Blocker where
  toJSON (BlockerId taskId) =
    -- TaskId's ToJSON derives from its underlying Text, so this is fine.
    toJSON $ angleBracket taskId.unTaskId
  toJSON (BlockerText t) = toJSON t

instance FromJSON Blocker where
  parseJSON = Asn.withText "Blocker" parseBlockedTarget

instance IsString Blocker where
  fromString s = case parseBlockedTarget (pack s) of
    EitherRight b -> b
    EitherLeft err -> error err

parseBlockedTarget :: (MonadFail m) => Text -> m Blocker
parseBlockedTarget orig@(T.stripPrefix "<" -> Just start) =
  case T.stripSuffix ">" start of
    Just txt -> BlockerId <$> TaskId.parseTaskId txt
    Nothing ->
      fail
        $ mconcat
          [ "Blocker task id started with '<' but did not end with '>': '",
            unpack orig,
            "'"
          ]
parseBlockedTarget other = case TaskId.mkA "Blocker text" BlockerText other of
  Left err -> fail err
  Right x -> pure x

-- | Task status. The (commutative) semigroup is based roughly  on the ordering
--
-- @
--   Completed < NotStarted < InProgress < Blocked
-- @
--
-- where we take the max. There are exceptionals cases for
--
-- @
--   Completed <> NotStarted == InProgress
--   Blocked l <> Blocked r == Blocked (l <> r)
-- @
--
-- The first line is why we do not have a monoid.
data TaskStatus
  = Completed
  | NotStarted
  | InProgress
  | -- | Task is blocked, where the blockers can be another task (represented
    -- by TaskId) or a general text description.
    Blocked (NESet Blocker)
  deriving stock (Eq, Ord, Show)

filterBlockingIds :: NESet Blocker -> Set TaskId
filterBlockingIds = NESet.foldl' f Set.empty
  where
    f acc (BlockerText _) = acc
    f acc (BlockerId id) = Set.insert id acc

instance Semigroup TaskStatus where
  Blocked xs <> Blocked ys = Blocked (xs <> ys)
  Completed <> NotStarted = InProgress
  NotStarted <> Completed = InProgress
  l <> r = max l r

instance FromJSON TaskStatus where
  parseJSON = Asn.withText "TaskStatus" parseTaskStatus

parseTaskStatus :: forall m. (MonadFail m) => Text -> m TaskStatus
parseTaskStatus txt = do
  foldMappersAltA txt parsers >>= \case
    Nothing -> fail $ "Unexpected status value: " <> quoteTxt txt
    Just r -> pure r
  where
    parsers :: List (Text -> m (Maybe TaskStatus))
    parsers =
      [ parseBlocked,
        parseExact "not-started" NotStarted,
        parseExact "in-progress" InProgress,
        parseExact "completed" Completed
      ]

    parseBlocked :: Text -> m (Maybe TaskStatus)
    parseBlocked (T.stripPrefix "blocked:" -> Just rest) =
      case T.strip rest of
        "" -> fail $ "Received empty list for 'blocked' status: " <> quoteTxt rest
        nonEmpty ->
          let splitStrs = T.strip <$> T.split (== ',') nonEmpty
              nonEmpties = filter (not . T.null) splitStrs
           in case nonEmpties of
                (x : xs) -> do
                  (y :<|| ys) <- traverse parseBlockedTarget (x :<|| listToSeq xs)
                  pure $ Just $ Blocked (NESet.fromList (y :| toList ys))
                [] -> fail $ "Received no non-empty blockers for 'blocked' status: " <> quoteTxt rest
    parseBlocked _ = pure Nothing

    parseExact :: Text -> TaskStatus -> Text -> m (Maybe TaskStatus)
    parseExact e c t =
      pure
        $ if e == t
          then Just c
          else Nothing

    quoteTxt :: Text -> String
    quoteTxt t = "'" <> unpack t <> "'"

instance ToJSON TaskStatus where
  toJSON (Blocked ts) = toJSON $ "blocked: " <> blockedToText ts
    where
      blockedToText :: NESet Blocker -> Text
      blockedToText = mapBlockedCustom identity (angleBracket . (.unTaskId))
  toJSON NotStarted = "not-started"
  toJSON InProgress = "in-progress"
  toJSON Completed = "completed"

-- | True iff Completed.
isCompleted :: TaskStatus -> Bool
isCompleted Completed = True
isCompleted _ = False

-- | Renders to Builder.
render :: ColorSwitch -> TaskStatus -> Builder
render ColorOff Completed = "completed"
render ColorOff InProgress = "in-progress"
render ColorOff (Blocked tids) =
  displayBuilder $ "blocked: " <> renderBlockers tids
render ColorOff NotStarted = "not-started"
render ColorOn Completed = Render.Utils.colorBuilder Pretty.Green "completed"
render ColorOn InProgress = Render.Utils.colorBuilder Pretty.Yellow "in-progress"
render ColorOn (Blocked tids) =
  Render.Utils.colorBuilder Pretty.Red $ "blocked: " <> renderBlockers tids
render ColorOn NotStarted = Render.Utils.colorBuilder Pretty.Cyan "not-started"

-- | taskIdsToTextCustom that single-quotes the text
renderBlockers :: NESet Blocker -> Text
renderBlockers = mapBlockedCustom quote (angleBracket . (.unTaskId))
  where
    quote t = "'" <> t <> "'"

-- | Comma separates TaskIds together, using the provided function.
mapBlockedCustom ::
  (Text -> Text) ->
  (TaskId -> Text) ->
  NESet Blocker ->
  Text
mapBlockedCustom onText onTaskId =
  T.intercalate ", "
    . fmap g
    . toList
  where
    g (BlockerText t) = onText t
    g (BlockerId tid) = onTaskId tid

-- TODO: This logic is a bit fragile, in the sense that we have invariants
-- spread across several functions...consider make this more principled
-- e.g. using optics' Iso to represent to/from Text with angle brackets.
angleBracket :: (IsString a, Semigroup a) => a -> a
angleBracket t = "<" <> t <> ">"

metavar :: (IsString a) => a
metavar = "(blocked: <blockers> | completed | in-progress | not-started)"

_Completed :: Prism' TaskStatus ()
_Completed =
  prism
    (const Completed)
    ( \case
        Completed -> Right ()
        other -> Left other
    )
{-# INLINE _Completed #-}

_NotStarted :: Prism' TaskStatus ()
_NotStarted =
  prism
    (const NotStarted)
    ( \case
        NotStarted -> Right ()
        other -> Left other
    )
{-# INLINE _NotStarted #-}

_InProgress :: Prism' TaskStatus ()
_InProgress =
  prism
    (const InProgress)
    ( \case
        InProgress -> Right ()
        other -> Left other
    )
{-# INLINE _InProgress #-}

_Blocked :: Prism' TaskStatus (NESet Blocker)
_Blocked =
  prism
    Blocked
    ( \case
        Blocked blockers -> Right blockers
        other -> Left other
    )
{-# INLINE _Blocked #-}

_BlockerId :: Prism' Blocker TaskId
_BlockerId =
  prism
    BlockerId
    ( \case
        BlockerId taskId -> Right taskId
        other -> Left other
    )
{-# INLINE _BlockerId #-}

_BlockerText :: Prism' Blocker Text
_BlockerText =
  prism
    BlockerText
    ( \case
        BlockerText txt -> Right txt
        other -> Left other
    )
{-# INLINE _BlockerText #-}
