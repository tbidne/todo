module Unit.Todo.Index (tests) where

import Data.Map.Strict qualified as Map
import Data.Set.NonEmpty qualified as NESet
import Todo.Index qualified as Index
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Todo.Index"
    [ testGetBlockingIds
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
