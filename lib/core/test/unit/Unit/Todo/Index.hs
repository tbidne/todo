module Unit.Todo.Index (tests) where

import Data.Map.Strict qualified as Map
import Data.Set.NonEmpty qualified as NESet
import Todo.Data.Task (SomeTask, _SomeTaskGroup, _SomeTaskSingle)
import Todo.Data.TaskPriority (TaskPriority (Normal))
import Todo.Index qualified as Index
import Todo.Index.Optics qualified as IndexO
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Todo.Index"
    [ testGetBlockingIds,
      testGetParentIds,
      testIndexTraversal,
      testIndexPredTraversal,
      testIndexSingleTraversal,
      testIndexGroupTraversal
    ]

testGetBlockingIds :: TestTree
testGetBlockingIds = testCase "Retrieves blocking ids" $ do
  index <- Index.readIndex examplePath
  let result = Index.getBlockingIds index

  expected @=? result
  where
    expected =
      Map.fromList
        [ ("fix_car", NESet.fromList ("ball" :| ["cleats", "groceries"])),
          ("groceries", NESet.fromList ("pack_bananas" :| [])),
          ("paycheck", NESet.fromList ("groceries" :| []))
        ]

testGetParentIds :: TestTree
testGetParentIds = testCase "Retrieves parent ids" $ do
  index <- Index.readIndex examplePath
  let result = Index.getParentIds index

  expected @=? result
  where
    expected =
      [ "groceries",
        "soccer_match",
        "equipment",
        "empty_group"
      ]

testIndexTraversal :: TestTree
testIndexTraversal = testCase "indexTraversal retrieves all ids" $ do
  index <- Index.readIndex examplePath
  expected @=? indexToIds index
  where
    indexToIds =
      toListOf
        ( IndexO.unverifyGetter
            % IndexO.indexTraversal
            % #taskId
            % #unTaskId
        )
        . Index.unverify
    expected =
      [ "haircut",
        "walk_dog",
        "groceries",
        "apples",
        "bananas",
        "fix_car",
        "paycheck",
        "soccer_match",
        "pack_bananas",
        "equipment",
        "cleats",
        "ball",
        "empty_group"
      ]

testIndexPredTraversal :: TestTree
testIndexPredTraversal = testCase "indexTraversal retrieves targeted ids" $ do
  index <- Index.readIndex examplePath
  expected @=? indexToIds index
  where
    indexToIds =
      toListOf
        ( IndexO.unverifyGetter
            % IndexO.indexPredTraversal p
            % #taskId
            % #unTaskId
        )
        . Index.unverify

    p :: SomeTask -> Bool
    p = (/= Normal) . view #priority

    expected =
      [ "haircut",
        "groceries",
        "bananas",
        "fix_car",
        "paycheck",
        "soccer_match"
      ]

testIndexSingleTraversal :: TestTree
testIndexSingleTraversal = testCase "indexTraversal retrieves all single task ids" $ do
  index <- Index.readIndex examplePath
  expected @=? indexToIds index
  where
    indexToIds =
      toListOf
        ( IndexO.unverifyGetter
            % IndexO.indexTraversal
            % _SomeTaskSingle
            % #taskId
            % #unTaskId
        )
        . Index.unverify
    expected =
      [ "haircut",
        "walk_dog",
        "apples",
        "bananas",
        "fix_car",
        "paycheck",
        "pack_bananas",
        "cleats",
        "ball"
      ]

testIndexGroupTraversal :: TestTree
testIndexGroupTraversal = testCase "indexTraversal retrieves all task group ids" $ do
  index <- Index.readIndex examplePath
  expected @=? indexToIds index
  where
    indexToIds =
      toListOf
        ( IndexO.unverifyGetter
            % IndexO.indexTraversal
            % _SomeTaskGroup
            % #taskId
            % #unTaskId
        )
        . Index.unverify
    expected =
      [ "groceries",
        "soccer_match",
        "equipment",
        "empty_group"
      ]
