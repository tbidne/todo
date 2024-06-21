module Unit.Todo.Data.Task (tests) where

import Data.Aeson qualified as Asn
import Data.ByteString.Lazy qualified as BSL
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.LocalTime
  ( LocalTime (LocalTime),
    TimeOfDay (TimeOfDay),
    ZonedTime (ZonedTime),
    utc,
  )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Todo.Data.Task
  ( SomeTask (MultiTask, SingleTask),
    Task
      ( MkTask,
        deadline,
        description,
        priority,
        status,
        taskId
      ),
    TaskGroup (MkTaskGroup, priority, status, subtasks, taskId),
  )
import Todo.Data.Task.TaskId (TaskId)
import Todo.Data.Task.TaskId qualified as TaskId
import Todo.Data.Task.TaskPriority (TaskPriority)
import Todo.Data.Task.TaskStatus
  ( TaskStatus
      ( Blocked,
        Completed,
        InProgress,
        NotStarted
      ),
  )
import Todo.Data.Timestamp (Timestamp (Date, Local, Zoned))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Todo.Data.Task"
    [ testJsonRoundtrip,
      testTaskJsonRoundtrip,
      testTaskStatusJsonRoundtrip
    ]

testJsonRoundtrip :: TestTree
testJsonRoundtrip = testPropertyNamed desc "testJsonRoundtrip" $ property $ do
  tasks <- forAll genSomeTaskList

  let encoded = BSL.toStrict $ Asn.encode tasks
      eDecoded = Asn.eitherDecodeStrict encoded

  annotateShow encoded

  case eDecoded of
    Left err -> do
      annotate err
      failure
    Right result -> tasks === result
  where
    desc = "JSON encoding round trips"

-- NOTE: While techincally testJsonRoundTrip includes the below tests as
-- special cases, we include them because debugging failures is _much_ easier
-- (i.e. the List SomeTask can be enormous because of the recursive nature of
-- TaskGroup). The hope is that a failure in testJsonRoundTrip will also
-- cause one of the simpler tests below to fail.

testTaskJsonRoundtrip :: TestTree
testTaskJsonRoundtrip = testPropertyNamed desc "testTaskJsonRoundtrip" $ property $ do
  task <- forAll genTask

  let encoded = BSL.toStrict $ Asn.encode task
      eDecoded = Asn.eitherDecodeStrict encoded

  annotateShow encoded

  case eDecoded of
    Left err -> do
      annotate err
      failure
    Right result -> task === result
  where
    desc = "Task JSON encoding round trips"

testTaskStatusJsonRoundtrip :: TestTree
testTaskStatusJsonRoundtrip = testPropertyNamed desc "testTaskStatusJsonRoundtrip" $ property $ do
  taskStatus <- forAll genTaskStatus

  let encoded = BSL.toStrict $ Asn.encode taskStatus
      eDecoded = Asn.eitherDecodeStrict encoded

  annotateShow encoded

  case eDecoded of
    Left err -> do
      annotate err
      failure
    Right result -> taskStatus === result
  where
    desc = "TaskStatus JSON encoding round trips"

genSomeTaskList :: Gen (List SomeTask)
genSomeTaskList = Gen.list (Range.linearFrom 0 0 20) genSomeTask

genSomeTask :: Gen SomeTask
genSomeTask =
  Gen.frequency
    [ (3, SingleTask <$> genTask), -- weigh single tasks to make tests faster
      (1, MultiTask <$> genTaskGroup)
    ]

genTaskGroup :: Gen TaskGroup
genTaskGroup = do
  priority <- Gen.maybe genTaskPriority
  status <- Gen.maybe genTaskStatus
  subtasks <- genSubtasks
  taskId <- genTaskId
  pure
    $ MkTaskGroup
      { priority,
        status,
        subtasks,
        taskId
      }
  where
    genSubtasks = NESeq.fromList <$> Gen.nonEmpty (Range.linearFrom 1 3 3) genSomeTask

genTask :: Gen Task
genTask = do
  deadline <- Gen.maybe genTimestamp
  description <- Gen.maybe genDescription
  priority <- genTaskPriority
  status <- genTaskStatus
  taskId <- genTaskId
  pure
    $ MkTask
      { deadline,
        description,
        priority,
        status,
        taskId
      }

genTimestamp :: Gen Timestamp
genTimestamp =
  Gen.choice
    [ Date <$> genDay,
      Local <$> genLocalTime,
      Zoned <$> genZonedTime
    ]
  where
    genDay = ModifiedJulianDay <$> genDate
    genLocalTime = LocalTime <$> genDay <*> genTimeOfDay
    genZonedTime = ZonedTime <$> genLocalTime <*> genTimeZone
    genDate = Gen.integral (Range.linearFrom 50_000 50_000 100_000) -- 1995 to 2105
    genTimeOfDay =
      TimeOfDay
        <$> Gen.integral (Range.linearFrom 0 0 23) -- hours
        <*> Gen.integral (Range.linearFrom 0 0 59) -- minutes
        -- NOTE: The "right" way to do this is:
        --
        --     (MkFixed <$> Gen.integral (Range.linearFrom 0 0 60)) -- pico (60 == leap second)
        --
        -- However, this does _not_ roundtrip because encoding rounds the
        -- nearest second. I don't actually care about that level of precision,
        -- hence sticking with 0.
        <*> pure 0
    genTimeZone = pure utc -- TODO: Maybe offer some way to add timezone config

genDescription :: Gen Text
genDescription = genText 100

genTaskStatus :: Gen TaskStatus
genTaskStatus =
  Gen.choice
    [ pure Completed,
      pure InProgress,
      pure NotStarted,
      Blocked . NESeq.fromList <$> genBlocked
    ]
  where
    genBlocked = Gen.nonEmpty (Range.linearFrom 1 1 20) genTaskId

genTaskPriority :: Gen TaskPriority
genTaskPriority = Gen.enumBounded

-- We do not allow whitespace because it is currently stripped when reading
-- Blocked TaskIds.
genTaskId :: Gen TaskId
genTaskId = genMassaged >>= TaskId.parseTaskId
  where
    genMassaged = Gen.filter (not . badTxt) genRaw
    genRaw = stripBad <$> Gen.text (Range.linearFrom 1 1 20) Gen.unicode

    badTxt = T.null
    stripBad = T.replace "," "" . T.strip

genText :: Int -> Gen Text
genText upperBound = Gen.text (Range.linearFrom 0 0 upperBound) Gen.unicode
