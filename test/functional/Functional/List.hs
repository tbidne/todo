module Functional.List (tests) where

import Functional.Prelude
import Todo.Index (BlockedIdRefE, DuplicateIdE)

tests :: TestTree
tests =
  testGroup
    "List"
    [ testExample,
      testExamplePriority,
      testExampleStatus,
      testExamplePriorityStatus,
      testExampleStatusPriority,
      testDuplicateIdFails,
      testRefBadIdFails
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

testDuplicateIdFails :: TestTree
testDuplicateIdFails = goldenVsString "Duplicate id fails" path $ do
  result <- runException @DuplicateIdE args
  pure $ toBSL result
  where
    args =
      [ "--path",
        inputDir `cfp` "dup_ids.json",
        "list",
        "--color",
        "off"
      ]

    path = outputDir `cfp` "dup_ids.golden"

testRefBadIdFails :: TestTree
testRefBadIdFails = goldenVsString "Non-extant id reference fails" path $ do
  result <- runException @BlockedIdRefE args
  pure $ toBSL result
  where
    args =
      [ "--path",
        inputDir `cfp` "ref_bad_id.json",
        "list",
        "--color",
        "off"
      ]

    path = outputDir `cfp` "ref_bad_id.golden"

inputDir :: FilePath
inputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "List" `cfp` "input"

outputDir :: FilePath
outputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "List" `cfp` "output"
