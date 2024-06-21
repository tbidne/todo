module Functional.List (tests) where

import Data.Aeson (AesonException)
import Functional.Prelude
import Todo.Index (BlockedIdRefE, DuplicateIdE)

tests :: TestTree
tests =
  testGroup
    "List"
    [ sortExampleTests,
      failureTests
    ]

sortExampleTests :: TestTree
sortExampleTests =
  testGroup
    "Example sorts"
    [ testExample,
      testExamplePriority,
      testExampleStatus,
      testExamplePriorityStatus,
      testExampleStatusPriority
    ]

testExample :: TestTree
testExample = goldenVsString "Default sort" path $ do
  result <- run args
  pure $ toBSL result
  where
    args =
      [ "--path",
        "examples" `cfp` "todo.json",
        "list",
        "--color",
        "off"
      ]

    path = outputDir `cfp` "example.golden"

testExamplePriority :: TestTree
testExamplePriority = goldenVsString "Sorted by priority" path $ do
  result <- run args
  pure $ toBSL result
  where
    args =
      [ "--path",
        "examples" `cfp` "todo.json",
        "list",
        "--color",
        "off",
        "--sort",
        "priority"
      ]

    path = outputDir `cfp` "example_priority.golden"

testExampleStatus :: TestTree
testExampleStatus = goldenVsString "Sorted by status" path $ do
  result <- run args
  pure $ toBSL result
  where
    args =
      [ "--path",
        "examples" `cfp` "todo.json",
        "list",
        "--color",
        "off",
        "--sort",
        "status"
      ]

    path = outputDir `cfp` "example_status.golden"

testExamplePriorityStatus :: TestTree
testExamplePriorityStatus = goldenVsString "Sorted by priority_status" path $ do
  result <- run args
  pure $ toBSL result
  where
    args =
      [ "--path",
        "examples" `cfp` "todo.json",
        "list",
        "--color",
        "off",
        "--sort",
        "priority_status"
      ]

    path = outputDir `cfp` "example_priority_status.golden"

testExampleStatusPriority :: TestTree
testExampleStatusPriority = goldenVsString "Sorted by status_priority" path $ do
  result <- run args
  pure $ toBSL result
  where
    args =
      [ "--path",
        "examples" `cfp` "todo.json",
        "list",
        "--color",
        "off",
        "--sort",
        "status_priority"
      ]

    path = outputDir `cfp` "example_status_priority.golden"

failureTests :: TestTree
failureTests =
  testGroup
    "Failures"
    [ testIdDupsFails,
      testIdEmptyFails,
      testIdCommaFails,
      testStatusBlockedBadRefFails,
      testStatusBadFails,
      testStatusBlockedEmptFails,
      testStatusBlockedIdsEmptyFails
    ]

testIdDupsFails :: TestTree
testIdDupsFails = goldenVsString "Duplicate id fails" path $ do
  result <- runException @DuplicateIdE args
  pure $ toBSL result
  where
    args =
      [ "--path",
        inputDir `cfp` "id_dups.json",
        "list",
        "--color",
        "off"
      ]

    path = outputDir `cfp` "id_dups.golden"

testIdEmptyFails :: TestTree
testIdEmptyFails = goldenVsString "Empty id fails" path $ do
  result <- runException @AesonException args
  pure $ toBSL result
  where
    args =
      [ "--path",
        inputDir `cfp` "id_empty.json",
        "list",
        "--color",
        "off"
      ]

    path = outputDir `cfp` "id_empty.golden"

testIdCommaFails :: TestTree
testIdCommaFails = goldenVsString "Id with comma fails" path $ do
  result <- runException @AesonException args
  pure $ toBSL result
  where
    args =
      [ "--path",
        inputDir `cfp` "id_comma.json",
        "list",
        "--color",
        "off"
      ]

    path = outputDir `cfp` "id_comma.golden"

testStatusBlockedBadRefFails :: TestTree
testStatusBlockedBadRefFails = goldenVsString desc path $ do
  result <- runException @BlockedIdRefE args
  pure $ toBSL result
  where
    args =
      [ "--path",
        inputDir `cfp` "status_blocked_bad_ref.json",
        "list",
        "--color",
        "off"
      ]
    desc = "Status blocked non-extant id reference fails"
    path = outputDir `cfp` "status_blocked_bad_ref.golden"

testStatusBadFails :: TestTree
testStatusBadFails = goldenVsString "Bad status fails" path $ do
  result <- runException @AesonException args
  pure $ toBSL result
  where
    args =
      [ "--path",
        inputDir `cfp` "status_bad.json",
        "list",
        "--color",
        "off"
      ]

    path = outputDir `cfp` "status_bad.golden"

testStatusBlockedEmptFails :: TestTree
testStatusBlockedEmptFails = goldenVsString "Blocked empty status fails" path $ do
  result <- runException @AesonException args
  pure $ toBSL result
  where
    args =
      [ "--path",
        inputDir `cfp` "status_blocked_empty.json",
        "list",
        "--color",
        "off"
      ]

    path = outputDir `cfp` "status_blocked_empty.golden"

testStatusBlockedIdsEmptyFails :: TestTree
testStatusBlockedIdsEmptyFails = goldenVsString "Blocked ids empty status fails" path $ do
  result <- runException @AesonException args
  pure $ toBSL result
  where
    args =
      [ "--path",
        inputDir `cfp` "status_blocked_ids_empty.json",
        "list",
        "--color",
        "off"
      ]

    path = outputDir `cfp` "status_blocked_ids_empty.golden"

inputDir :: FilePath
inputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "List" `cfp` "input"

outputDir :: FilePath
outputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "List" `cfp` "output"
