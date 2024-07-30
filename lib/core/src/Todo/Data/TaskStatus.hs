module Todo.Data.TaskStatus
  ( -- * Status
    TaskStatus (..),
    parseTaskStatus,
    filterBlockingIds,
    isCompleted,
    metavar,

    -- ** Optics
    _Completed,
    _NotStarted,
    _InProgress,
    _Blocked,
    statusIso,
    StatusMatch (..),

    -- * Blocker
    Blocker (..),

    -- ** Optics
    _BlockerId,
    _BlockerText,

    -- ** Misc
    blockersToText,
  )
where

import Data.Aeson qualified as Asn
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as T
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Prelude
import Todo.Utils qualified as Utils

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
parseTaskStatus txt = case review statusIso txt of
  StatusMatchError err -> fail $ unpack err
  StatusMatchSuccess x -> pure x

instance ToJSON TaskStatus where
  toJSON = toJSON . view statusIso . StatusMatchSuccess

-- | True iff Completed.
isCompleted :: TaskStatus -> Bool
isCompleted Completed = True
isCompleted _ = False

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

-- | Used in 'statusIso'.
data StatusMatch
  = StatusMatchError Text
  | StatusMatchSuccess TaskStatus
  deriving stock (Eq, Show)

-- | Iso from Text <-> Status, intended for centralizing parsing /
-- display. Note that we do not distinguish between text / id parse errors,
-- though we easily can if the need arises.
--
-- __WARNING:__ This is not actually an isomorphism! This is due to us using
-- the @Text -> Status@ function to report parsing errors with a custom
-- error message. In other words, the following law:
--
-- @
--     view statusIso (review statusIso txt) === txt
-- @
--
-- does __NOT__ hold. This is fine since we only use statusIso for simple
-- parsing / display, not algebraic reasoning.
statusIso :: Iso' StatusMatch Text
statusIso =
  iso
    ( \case
        StatusMatchError err -> err
        StatusMatchSuccess Completed -> "completed"
        StatusMatchSuccess InProgress -> "in-progress"
        StatusMatchSuccess NotStarted -> "not-started"
        StatusMatchSuccess (Blocked blockers) ->
          "blocked: " <> blockersToText blockers
    )
    ( \case
        "completed" -> StatusMatchSuccess Completed
        "in-progress" -> StatusMatchSuccess InProgress
        "not-started" -> StatusMatchSuccess NotStarted
        other -> case T.stripPrefix "blocked:" other of
          Just rest ->
            case T.strip rest of
              "" -> StatusMatchError $ "Received empty list for 'blocked' status: " <> quoteTxt rest
              nonEmpty ->
                let splitStrs = T.strip <$> T.split (== ',') nonEmpty
                    nonEmpties = filter (not . T.null) splitStrs
                 in case nonEmpties of
                      (x : xs) -> do
                        case traverse parseBlockedTarget (x :| xs) of
                          EitherLeft err -> StatusMatchError $ pack err
                          EitherRight (y :| ys) ->
                            StatusMatchSuccess $ Blocked (NESet.fromList (y :| ys))
                      [] -> StatusMatchError $ "Received no non-empty blockers for 'blocked' status: " <> quoteTxt rest
          Nothing -> StatusMatchError $ "Unexpected status value: " <> quoteTxt other
    )
  where
    quoteTxt t = "'" <> t <> "'"

-- | Something that blocks another task.
data Blocker
  = -- | A task that blocks another task. The task is is the blocker.
    BlockerId TaskId
  | -- | General description that blocks a task.
    BlockerText Text
  deriving stock (Eq, Ord, Show)

instance IsString Blocker where
  fromString s = case parseBlockedTarget (pack s) of
    EitherRight b -> b
    EitherLeft err -> error err

parseBlockedTarget :: (MonadFail m) => Text -> m Blocker
parseBlockedTarget txt = case review blockerIso txt of
  (BlockerMatchSuccess b) -> pure b
  (BlockerError err) -> fail $ unpack err

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

-- | Used in 'blockerIso'.
data BlockerMatch
  = BlockerError Text
  | BlockerMatchSuccess Blocker
  deriving stock (Eq, Show)

-- | __WARNING:__ not an isomorphism for the same reasons as 'statusIso'.
blockerIso :: Iso' BlockerMatch Text
blockerIso =
  iso
    ( \case
        BlockerError err -> err
        BlockerMatchSuccess (BlockerId tid) -> angleBracket tid.unTaskId
        BlockerMatchSuccess (BlockerText t) -> t
    )
    ( \t -> case T.stripPrefix "<" t of
        Just start -> case T.stripSuffix ">" start of
          Nothing ->
            BlockerError
              $ mconcat
                [ "Blocker task id started with '<' but did not end with '>': '",
                  t,
                  "'"
                ]
          Just idText -> case TaskId.parseTaskId idText of
            EitherLeft err -> BlockerError $ pack err
            EitherRight tid -> BlockerMatchSuccess $ BlockerId tid
        Nothing -> case TaskId.mkA "Blocker text" BlockerText t of
          Left err -> BlockerError $ pack err
          Right x -> BlockerMatchSuccess x
    )
  where
    angleBracket t = "<" <> t <> ">"

blockersToText :: NESet Blocker -> Text
blockersToText = Utils.foldableToText (view blockerIso . BlockerMatchSuccess)
