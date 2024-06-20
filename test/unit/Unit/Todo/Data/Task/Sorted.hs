module Unit.Todo.Data.Task.Sorted (tests) where

import Hedgehog.Gen qualified as G
import Todo.Data.Task
  ( SomeTask,
    Task (taskId),
    TaskGroup (taskId),
  )
import Todo.Data.Task.Sorted
  ( SortType
      ( SortPriority,
        SortPriorityStatus,
        SortStatus,
        SortStatusPriority
      ),
    SortedTasks,
  )
import Todo.Data.Task.Sorted qualified as Sorted
import Todo.Data.Task.TaskId (TaskId (unTaskId))
import Todo.Index (Index (unIndex))
import Todo.Index qualified as Index
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Todo.Data.Task.Sorted"
    [ exampleTests
    ]

exampleTests :: TestTree
exampleTests =
  testGroup
    "Example sorting is deterministic and permutation-invariant"
    [ testExample,
      testExamplePriority,
      testExampleStatus,
      testExamplePriorityStatus,
      testExampleStatusPriority
    ]

testExample :: TestTree
testExample =
  testExampleSort
    "Example"
    "testExample"
    Nothing
    [ "groceries",
      "bananas",
      "apples",
      "soccer_match",
      "pack_bananas",
      "equipment",
      "cleats",
      "ball",
      "fix_car",
      "haircut",
      "paycheck",
      "walk_dog"
    ]

testExamplePriority :: TestTree
testExamplePriority =
  testExampleSort
    "Example sorted by Priority"
    "testExamplePriority"
    (Just SortPriority)
    [ "fix_car",
      "groceries",
      "bananas",
      "apples",
      "paycheck",
      "walk_dog",
      "haircut",
      "soccer_match",
      "equipment",
      "cleats",
      "ball",
      "pack_bananas"
    ]

testExampleStatus :: TestTree
testExampleStatus =
  testExampleSort
    "Example sorted by Status"
    "testExampleStatus"
    (Just SortStatus)
    [ "groceries",
      "apples",
      "bananas",
      "soccer_match",
      "pack_bananas",
      "equipment",
      "cleats",
      "ball",
      "fix_car",
      "haircut",
      "paycheck",
      "walk_dog"
    ]

testExamplePriorityStatus :: TestTree
testExamplePriorityStatus =
  testExampleSort
    "Example sorted by (Priority, Status)"
    "testExamplePriorityStatus"
    (Just SortPriorityStatus)
    [ "groceries",
      "bananas",
      "apples",
      "fix_car",
      "paycheck",
      "walk_dog",
      "soccer_match",
      "pack_bananas",
      "equipment",
      "cleats",
      "ball",
      "haircut"
    ]

testExampleStatusPriority :: TestTree
testExampleStatusPriority =
  testExampleSort
    "Example sorted by (Status, Priority)"
    "testExampleStatusPriority"
    (Just SortStatusPriority)
    [ "groceries",
      "bananas",
      "apples",
      "soccer_match",
      "pack_bananas",
      "equipment",
      "cleats",
      "ball",
      "fix_car",
      "haircut",
      "paycheck",
      "walk_dog"
    ]

testExampleSort ::
  String ->
  PropertyName ->
  Maybe SortType ->
  List Text ->
  TestTree
testExampleSort
  desc
  name
  mSortType
  expected = testPropertyNamed desc name $ property $ do
    xs <- liftIO getExampleList
    ys <- forAll (G.shuffle xs)
    let sorted = Sorted.sortTasks mSortType ys
        ids = getIds sorted

    annotateShow sorted

    expected === ids

getExampleList :: IO (List SomeTask)
getExampleList = (.unIndex) <$> Index.readIndex examplePath

getIds :: SortedTasks -> List Text
getIds = Sorted.traverseSorted (.taskId.unTaskId) (.taskId.unTaskId)
