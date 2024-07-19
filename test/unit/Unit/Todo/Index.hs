module Unit.Todo.Index (tests) where

import Data.Map.Strict qualified as Map
import Data.Set.NonEmpty qualified as NESet
import Todo.Data.Task (SomeTask)
import Todo.Data.TaskPriority (TaskPriority (Normal))
import Todo.Index qualified as Index
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Todo.Index"
    [ testGetBlockingIds,
      testIndexTraversal,
      testIndexPredTraversal
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

testIndexTraversal :: TestTree
testIndexTraversal = testCase "indexTraversal retrieves all ids" $ do
  index <- Index.readIndex examplePath
  expected @=? indexToIds index
  where
    indexToIds = toListOf (Index.indexTraversal % #taskId % #unTaskId)
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
    indexToIds = toListOf (Index.indexPredTraversal p % #taskId % #unTaskId)

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
