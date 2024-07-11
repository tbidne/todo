{-# LANGUAGE QuasiQuotes #-}

module Unit.Todo.Data.Sorted (tests) where

import Hedgehog.Gen qualified as G
import Todo.Data.Sorted
  ( SortType
      ( SortPriority,
        SortPriorityStatus,
        SortStatus,
        SortStatusPriority
      ),
    SortedTasks,
  )
import Todo.Data.Sorted qualified as Sorted
import Todo.Data.Task
  ( SingleTask (taskId),
    SomeTask,
    TaskGroup (taskId),
  )
import Todo.Index qualified as Index
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Todo.Data.Sorted"
    [ exampleTests,
      otherTests
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
      "fix_car",
      "paycheck",
      "soccer_match",
      "pack_bananas",
      "equipment",
      "ball",
      "cleats",
      "haircut",
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
      "ball",
      "cleats",
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
      "ball",
      "cleats",
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
      "ball",
      "cleats",
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
      "ball",
      "cleats",
      "fix_car",
      "haircut",
      "paycheck",
      "walk_dog"
    ]

otherTests :: TestTree
otherTests =
  testGroup
    "Other sorting is deterministic and permutation-invariant"
    [ testNested
    ]

testNested :: TestTree
testNested =
  testSort
    (getList path)
    "Nested groups sorted by default"
    "testNested"
    Nothing
    [ "some_group",
      "nested_group",
      "nested_subtask2",
      "nested_subtask3",
      "nested_subtask1",
      "subtask2",
      "subtask1",
      "some_task",
      "completed_task"
    ]
  where
    path =
      [osp|test|]
        </> [osp|unit|]
        </> [osp|Unit|]
        </> [osp|Todo|]
        </> [osp|Data|]
        </> [osp|Sorted|]
        </> [osp|nested_sort.json|]

testExampleSort ::
  String ->
  PropertyName ->
  Maybe SortType ->
  List Text ->
  TestTree
testExampleSort = testSort getExampleList

testSort ::
  IO (List SomeTask) ->
  String ->
  PropertyName ->
  Maybe SortType ->
  List Text ->
  TestTree
testSort
  getTasks
  desc
  name
  mSortType
  expected = testPropertyNamed desc name $ property $ do
    xs <- liftIO getTasks
    ys <- forAll (G.shuffle xs)
    let sorted = Sorted.sortTasks mSortType ys
        ids = getIds sorted

    annotateShow ids

    expected === ids

getExampleList :: IO (List SomeTask)
getExampleList = getList examplePath

getList :: OsPath -> IO (List SomeTask)
getList path = (.taskList) <$> Index.readIndex path

getIds :: SortedTasks -> List Text
getIds = Sorted.traverseSorted (.taskId.unTaskId) (.taskId.unTaskId)
