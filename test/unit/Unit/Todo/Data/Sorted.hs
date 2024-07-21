{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Todo.Data.Sorted (tests) where

import Data.Sequence qualified as Seq
import Hedgehog.Gen qualified as G
import Todo.Configuration.Data.RevSort (RevSort (RevSortOff, RevSortOn))
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
import Todo.Data.Sorted.Internal (SortedTasks (UnsafeSortedTasks))
import Todo.Data.Task
  ( SingleTask (taskId),
    SomeTask (SomeTaskGroup, SomeTaskSingle),
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
      "paycheck",
      "fix_car",
      "soccer_match",
      "pack_bananas",
      "equipment",
      "ball",
      "cleats",
      "haircut",
      "empty_group",
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
      "empty_group",
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
      "paycheck",
      "fix_car",
      "haircut",
      "empty_group",
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
      "paycheck",
      "fix_car",
      "empty_group",
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
      "paycheck",
      "fix_car",
      "haircut",
      "empty_group",
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
  Seq Text ->
  TestTree
testExampleSort = testSort getExampleList

testSort ::
  IO (Seq SomeTask) ->
  String ->
  PropertyName ->
  Maybe SortType ->
  Seq Text ->
  TestTree
testSort
  getTasks
  desc
  name
  mSortType
  expected = testPropertyNamed desc name $ property $ do
    xs <- liftIO getTasks
    ys <- forAll (listToSeq <$> G.shuffle (seqToList xs))
    let sorted = Sorted.sortTasks mSortType RevSortOff ys
        revSorted = Sorted.sortTasks mSortType RevSortOn ys
        sortedIds = getIds sorted
        revSortedIds = getIds revSorted

        -- Reversing the expected isn't completely trivial (L.reverse sortedIds)
        -- since the task group ids are __not__ reversed w.r.t. their groups.
        -- That is, the group id always precedes the subtasks. Thus we use
        -- the following function, which reverses all tasks except the
        -- groups.
        revExpected = reverseSorted sorted
        revExpectedIds = getIds revExpected

    -- extra print here as the result is easier to copy/paste than
    -- the (===) output, when we need to update the results
    annotateShow sortedIds
    annotateShow revSortedIds

    expected === sortedIds
    revExpectedIds === revSortedIds

reverseSorted :: SortedTasks -> SortedTasks
reverseSorted = UnsafeSortedTasks . (Seq.reverse . fmap go) . (.unSortedTasks)
  where
    go st@(SomeTaskSingle _) = st
    go (SomeTaskGroup t) =
      SomeTaskGroup $ over' #subtasks (Seq.reverse . fmap go) t

getExampleList :: IO (Seq SomeTask)
getExampleList = getList examplePath

getList :: OsPath -> IO (Seq SomeTask)
getList path = (.taskList) <$> Index.readIndex path

getIds :: SortedTasks -> Seq Text
getIds = Sorted.traverseSorted (.taskId.unTaskId) (.taskId.unTaskId)
