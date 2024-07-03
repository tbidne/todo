module Unit.Todo.Data.Task (tests) where

import Data.Aeson qualified as Asn
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (any)
import Data.Sequence qualified as Seq
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as T
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.LocalTime
  ( LocalTime (LocalTime),
    TimeOfDay (TimeOfDay),
    ZonedTime (ZonedTime),
    utc,
  )
import Hedgehog (assert)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Todo.Data.Task
  ( SomeTask (SomeTaskGroup, SomeTaskSingle),
    SingleTask
      ( MkSingleTask,
        deadline,
        description,
        priority,
        status,
        taskId
      ),
    TaskGroup (MkTaskGroup, priority, status, subtasks, taskId),
  )
import Todo.Data.Task qualified as Task
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Data.TaskPriority (TaskPriority (High, Low, Normal))
import Todo.Data.TaskStatus
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
    [ taskGroupTasks,
      jsonTests
    ]

taskGroupTasks :: TestTree
taskGroupTasks =
  testGroup
    "TaskGroup"
    [ testTaskGroupUsesSetStatus tasks,
      testTaskGroupDerivesStatus tasks,
      testTaskGroupEmptyDerivesCompleted,
      testTaskGroupUsesSetPriority tasks,
      testTaskGroupNoDerivesPriority tasks
    ]
  where
    t1 =
      MkSingleTask
        { taskId = TaskId.unsafeTaskId "t1",
          priority = Low,
          status = InProgress,
          deadline = Nothing,
          description = Nothing
        }
    t2 =
      MkSingleTask
        { taskId = TaskId.unsafeTaskId "t2",
          priority = Normal,
          status = NotStarted,
          deadline = Nothing,
          description = Nothing
        }
    t3 =
      MkSingleTask
        { taskId = TaskId.unsafeTaskId "t3",
          priority = High,
          status = Completed,
          deadline = Nothing,
          description = Nothing
        }
    tasks = SomeTaskSingle t1 :<| SomeTaskSingle t2 :<| SomeTaskSingle t3 :<| Empty

testTaskGroupUsesSetStatus :: Seq SomeTask -> TestTree
testTaskGroupUsesSetStatus subtasks = testCase "TaskGroup uses set status" $ do
  let result = Task.taskGroupStatus taskGroup
  Completed @=? result
  where
    taskGroup =
      MkTaskGroup
        { taskId = TaskId.unsafeTaskId "tg",
          priority = Nothing,
          status = Just Completed,
          subtasks
        }

testTaskGroupDerivesStatus :: Seq SomeTask -> TestTree
testTaskGroupDerivesStatus subtasks = testCase "TaskGroup derives status" $ do
  let result = Task.taskGroupStatus taskGroup
  -- NotStarted is the greatest subtask status
  NotStarted @=? result
  where
    taskGroup =
      MkTaskGroup
        { taskId = TaskId.unsafeTaskId "tg",
          priority = Nothing,
          status = Nothing,
          subtasks
        }

testTaskGroupEmptyDerivesCompleted :: TestTree
testTaskGroupEmptyDerivesCompleted = testCase "TaskGroup empty derives completed" $ do
  let result = Task.taskGroupStatus taskGroup
  -- Completed is the least subtask status
  Completed @=? result
  where
    taskGroup =
      MkTaskGroup
        { taskId = TaskId.unsafeTaskId "tg",
          priority = Nothing,
          status = Nothing,
          subtasks = Empty
        }

testTaskGroupUsesSetPriority :: Seq SomeTask -> TestTree
testTaskGroupUsesSetPriority subtasks = testCase "TaskGroup uses set priority" $ do
  let result = taskGroup.priority
  Just Low @=? result
  where
    taskGroup =
      MkTaskGroup
        { taskId = TaskId.unsafeTaskId "tg",
          priority = Just Low,
          status = Nothing,
          subtasks
        }

testTaskGroupNoDerivesPriority :: Seq SomeTask -> TestTree
testTaskGroupNoDerivesPriority subtasks = testCase "TaskGroup does not derive priority" $ do
  let result = taskGroup.priority
  Nothing @=? result
  where
    taskGroup =
      MkTaskGroup
        { taskId = TaskId.unsafeTaskId "tg",
          priority = Nothing,
          status = Nothing,
          subtasks
        }

jsonTests :: TestTree
jsonTests =
  testGroup
    "JSON"
    [ testJsonRoundtrip,
      testTaskJsonRoundtrip,
      testTaskStatusJsonRoundtrip
    ]

testJsonRoundtrip :: TestTree
testJsonRoundtrip = testPropertyNamed desc "testJsonRoundtrip" $ property $ do
  tasks <- forAll genSomeTaskList

  let asnValue = Asn.toJSON tasks
      encoded = BSL.toStrict $ Asn.encode tasks
      eDecoded = Asn.eitherDecodeStrict encoded

  annotateShow asnValue
  annotateShow encoded

  assert $ not $ containsNull asnValue

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

  let asnValue = Asn.toJSON task
      encoded = BSL.toStrict $ Asn.encode task
      eDecoded = Asn.eitherDecodeStrict encoded

  annotateShow asnValue
  annotateShow encoded

  assert $ not $ containsNull asnValue

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
    [ (3, SomeTaskSingle <$> genTask), -- weigh single tasks to make tests faster
      (1, SomeTaskGroup <$> genTaskGroup)
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
    genSubtasks = Seq.fromList <$> Gen.list (Range.linearFrom 0 3 3) genSomeTask

genTask :: Gen SingleTask
genTask = do
  deadline <- Gen.maybe genTimestamp
  description <- Gen.maybe genDescription
  priority <- genTaskPriority
  status <- genTaskStatus
  taskId <- genTaskId
  pure
    $ MkSingleTask
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
      Blocked . NESet.fromList <$> genBlocked
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

containsNull :: Value -> Bool
containsNull Null = True
containsNull (Asn.Object km) = any containsNull km
containsNull (Asn.Array _) = False
containsNull (Asn.Bool _) = False
containsNull (Asn.Number _) = False
containsNull (Asn.String _) = False
