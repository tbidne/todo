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
  ( SingleTask
      ( MkSingleTask,
        deadline,
        description,
        priority,
        status,
        taskId
      ),
    SomeTask (SomeTaskGroup, SomeTaskSingle),
    TaskGroup (MkTaskGroup, priority, status, subtasks, taskId),
  )
import Todo.Data.Task qualified as Task
import Todo.Data.Task.Optics qualified as TaskO
import Todo.Data.TaskId (TaskId)
import Todo.Data.TaskId qualified as TaskId
import Todo.Data.TaskPriority (TaskPriority (High, Low, Normal))
import Todo.Data.TaskStatus
  ( Blocker (BlockerId, BlockerText),
    TaskStatus
      ( Blocked,
        Completed,
        InProgress,
        NotStarted
      ),
    _Completed,
  )
import Todo.Data.Timestamp (Timestamp (Date, Local, Zoned))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Todo.Data.Task"
    [ taskGroupTests,
      jsonTests,
      opticsTests
    ]

taskGroupTests :: TestTree
taskGroupTests =
  testGroup
    "TaskGroup"
    [ testTaskGroupUsesSetStatus tasks,
      testTaskGroupDerivesStatus tasks,
      testTaskGroupEmptyDerivesCompleted,
      testTaskGroupDerivesNotStarted,
      testTaskGroupUsesSetPriority tasks,
      testTaskGroupDerivesPriority tasks,
      testTaskGroupDerivesHigh,
      testTaskGroupDerivesLow
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
  let result = taskGroup.status
  Completed @=? result
  where
    taskGroup =
      SomeTaskGroup
        $ MkTaskGroup
          { taskId = TaskId.unsafeTaskId "tg",
            priority = Nothing,
            status = Just Completed,
            subtasks
          }

testTaskGroupDerivesStatus :: Seq SomeTask -> TestTree
testTaskGroupDerivesStatus subtasks = testCase "TaskGroup derives status" $ do
  let result = taskGroup.status
  InProgress @=? result
  where
    taskGroup =
      SomeTaskGroup
        $ MkTaskGroup
          { taskId = TaskId.unsafeTaskId "tg",
            priority = Nothing,
            status = Nothing,
            subtasks
          }

testTaskGroupEmptyDerivesCompleted :: TestTree
testTaskGroupEmptyDerivesCompleted = testCase "TaskGroup empty derives completed" $ do
  let result = taskGroup.status
  -- Completed is the default for empty
  Completed @=? result
  where
    taskGroup =
      SomeTaskGroup
        $ MkTaskGroup
          { taskId = TaskId.unsafeTaskId "tg",
            priority = Nothing,
            status = Nothing,
            subtasks = Empty
          }

testTaskGroupDerivesNotStarted :: TestTree
testTaskGroupDerivesNotStarted = testCase "TaskGroup not-started derives not-started" $ do
  let result = taskGroup.status
  NotStarted @=? result
  where
    taskGroup =
      SomeTaskGroup
        $ MkTaskGroup
          { taskId = TaskId.unsafeTaskId "tg",
            priority = Nothing,
            status = Nothing,
            subtasks = t1 :<| t2 :<| Empty
          }

    t1 =
      SomeTaskSingle
        $ MkSingleTask
          { taskId = TaskId.unsafeTaskId "t1",
            priority = Low,
            status = NotStarted,
            deadline = Nothing,
            description = Nothing
          }

    t2 =
      SomeTaskSingle
        $ MkSingleTask
          { taskId = TaskId.unsafeTaskId "t2",
            priority = Low,
            status = NotStarted,
            deadline = Nothing,
            description = Nothing
          }

testTaskGroupUsesSetPriority :: Seq SomeTask -> TestTree
testTaskGroupUsesSetPriority subtasks = testCase "TaskGroup uses set priority" $ do
  let result = taskGroup.priority
  Low @=? result
  where
    taskGroup =
      SomeTaskGroup
        $ MkTaskGroup
          { taskId = TaskId.unsafeTaskId "tg",
            priority = Just Low,
            status = Nothing,
            subtasks
          }

testTaskGroupDerivesPriority :: Seq SomeTask -> TestTree
testTaskGroupDerivesPriority subtasks = testCase "TaskGroup derives priority" $ do
  let result = taskGroup.priority
  -- Normal because only High task is Completed
  Normal @=? result
  where
    taskGroup =
      SomeTaskGroup
        $ MkTaskGroup
          { taskId = TaskId.unsafeTaskId "tg",
            priority = Nothing,
            status = Nothing,
            subtasks
          }

testTaskGroupDerivesHigh :: TestTree
testTaskGroupDerivesHigh = testCase "TaskGroup derives priority high" $ do
  let result = taskGroup.priority
  High @=? result
  where
    taskGroup =
      SomeTaskGroup
        $ MkTaskGroup
          { taskId = TaskId.unsafeTaskId "tg",
            priority = Nothing,
            status = Nothing,
            subtasks = t1 :<| t2 :<| t3 :<| Empty
          }

    t1 =
      SomeTaskSingle
        $ MkSingleTask
          { taskId = TaskId.unsafeTaskId "t1",
            priority = Low,
            status = NotStarted,
            deadline = Nothing,
            description = Nothing
          }

    t2 =
      SomeTaskSingle
        $ MkSingleTask
          { taskId = TaskId.unsafeTaskId "t2",
            priority = Normal,
            status = NotStarted,
            deadline = Nothing,
            description = Nothing
          }

    t3 =
      SomeTaskSingle
        $ MkSingleTask
          { taskId = TaskId.unsafeTaskId "t3",
            priority = High,
            status = NotStarted,
            deadline = Nothing,
            description = Nothing
          }

testTaskGroupDerivesLow :: TestTree
testTaskGroupDerivesLow = testCase "TaskGroup derives priority low" $ do
  let result = taskGroup.priority
  Low @=? result
  where
    taskGroup =
      SomeTaskGroup
        $ MkTaskGroup
          { taskId = TaskId.unsafeTaskId "tg",
            priority = Nothing,
            status = Nothing,
            subtasks = t1 :<| t2 :<| Empty
          }

    t1 =
      SomeTaskSingle
        $ MkSingleTask
          { taskId = TaskId.unsafeTaskId "t1",
            priority = Low,
            status = NotStarted,
            deadline = Nothing,
            description = Nothing
          }

    t2 =
      SomeTaskSingle
        $ MkSingleTask
          { taskId = TaskId.unsafeTaskId "t2",
            priority = Low,
            status = NotStarted,
            deadline = Nothing,
            description = Nothing
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
genSomeTaskList = Gen.list (Range.linearFrom 0 0 5) genSomeTask

genSomeTask :: Gen SomeTask
genSomeTask =
  Gen.frequency
    [ (5, SomeTaskSingle <$> genTask), -- weigh single tasks to make tests faster
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
    genSubtasks = Seq.fromList <$> Gen.list (Range.linear 0 2) genSomeTask

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
    genTimeZone = pure utc -- see TODO: [Toml configuration]

genDescription :: Gen Text
genDescription = genText 100

genTaskStatus :: Gen TaskStatus
genTaskStatus =
  Gen.choice
    [ pure Completed,
      pure InProgress,
      pure NotStarted,
      Blocked . NESet.fromList <$> genBlockers
    ]
  where
    genBlockers = Gen.nonEmpty (Range.linearFrom 1 1 20) genBlocker

genBlocker :: Gen Blocker
genBlocker =
  Gen.choice
    [ BlockerId <$> genTaskId,
      BlockerText <$> genBlockerText
    ]

genTaskPriority :: Gen TaskPriority
genTaskPriority = Gen.enumBounded

-- We do not allow whitespace because it is currently stripped when reading
-- Blocked TaskIds.
genTaskId :: Gen TaskId
genTaskId = genBlockerText >>= TaskId.parseTaskId

genBlockerText :: Gen Text
genBlockerText = genMassaged
  where
    genMassaged = Gen.filter (not . badTxt) genRaw
    genRaw = stripBad <$> genTextNE 20

    badTxt = T.null
    stripBad =
      T.replace ">" ""
        . T.replace "<" ""
        . T.replace "," ""
        . T.strip

genTextNE :: Int -> Gen Text
genTextNE = genTextBounds 1

genText :: Int -> Gen Text
genText = genTextBounds 0

genTextBounds :: Int -> Int -> Gen Text
genTextBounds lowerBound upperBound =
  Gen.text
    (Range.linear lowerBound upperBound)
    Gen.unicode

containsNull :: Value -> Bool
containsNull Null = True
containsNull (Asn.Object km) = any containsNull km
containsNull (Asn.Array _) = False
containsNull (Asn.Bool _) = False
containsNull (Asn.Number _) = False
containsNull (Asn.String _) = False

opticsTests :: TestTree
opticsTests =
  testGroup
    "Optics"
    [ testSomeTaskPredTraversalExample,
      testSomeTaskTraversal,
      testTaskGroupTraversalExample,
      testTaskGroupTraversal
    ]

testSomeTaskPredTraversalExample :: TestTree
testSomeTaskPredTraversalExample = testCase desc $ do
  let result = toListOf getIds exampleSomeTask

  ["t12", "g2", "t21"] @=? result
  where
    desc = "someTaskPredTraversal targets example predicate"
    getIds =
      TaskO.someTaskPredTraversal isCompleted % #taskId % #unTaskId

    isCompleted :: SomeTask -> Bool
    isCompleted = is (TaskO.someTaskStatusATraversal % _Completed)

testSomeTaskTraversal :: TestTree
testSomeTaskTraversal = testPropertyNamed desc "testSomeTaskTraversal" $ property $ do
  task <- forAll genSomeTask

  -- Test the optics' traversal in terms of the direct traverseSomeTasks.
  let expected = Task.traverseSomeTasks (.taskId.unTaskId) (.taskId.unTaskId) (Seq.singleton task)
      result = toListLikeOf getIds task

  expected === result
  where
    desc = "someTaskTraversal targets all tasks"
    getIds = TaskO.someTaskTraversal % #taskId % #unTaskId

testTaskGroupTraversalExample :: TestTree
testTaskGroupTraversalExample = testCase desc $ do
  let result = toListOf getIds exampleSomeTask

  ["g0", "g1", "g11", "g2"] @=? result
  where
    desc = "taskGroupTraversal targets example groups"
    getIds = TaskO.taskGroupTraversal % #taskId % #unTaskId

testTaskGroupTraversal :: TestTree
testTaskGroupTraversal = testPropertyNamed desc "testSomeTaskTraversal" $ property $ do
  task <- forAll genSomeTask

  -- HACK: We want to test that the optic traversal only returns the
  -- group ids. We do this by setting the single tasks ids to the empty string,
  -- then filtering them out below. This relies on genSomeTask generating
  -- non-empty ids.
  let expected = Task.traverseSomeTasks (const "") (.taskId.unTaskId) (Seq.singleton task)
      result = toListLikeOf getIds task

  Seq.filter (not . T.null) expected === result
  where
    desc = "taskGroupTraversal targets groups"
    getIds = TaskO.taskGroupTraversal % #taskId % #unTaskId

exampleSomeTask :: SomeTask
exampleSomeTask = SomeTaskGroup $ MkTaskGroup Nothing Nothing (g1 :<| g2 :<| Empty) "g0"
  where
    g1 = SomeTaskGroup $ MkTaskGroup Nothing Nothing (t11 :<| t12 :<| g11 :<| Empty) "g1"
    t11 = SomeTaskSingle $ MkSingleTask Nothing Nothing Low NotStarted "t11"
    t12 = SomeTaskSingle $ MkSingleTask Nothing Nothing Low Completed "t12"

    g11 = SomeTaskGroup $ MkTaskGroup Nothing Nothing Empty "g11"

    g2 = SomeTaskGroup $ MkTaskGroup Nothing (Just Completed) (t21 :<| t22 :<| Empty) "g2"
    t21 = SomeTaskSingle $ MkSingleTask Nothing Nothing Low Completed "t21"
    t22 = SomeTaskSingle $ MkSingleTask Nothing Nothing Low InProgress "t22"
