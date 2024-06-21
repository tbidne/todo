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
testExample = testGoldenExample "Default sort" Nothing "example"

testExamplePriority :: TestTree
testExamplePriority =
  testGoldenExample
    "Sorted by priority"
    (Just "priority")
    "example_priority"

testExampleStatus :: TestTree
testExampleStatus =
  testGoldenExample
    "Sorted by status"
    (Just "status")
    "example_status"

testExamplePriorityStatus :: TestTree
testExamplePriorityStatus =
  testGoldenExample
    "Sorted by priority_status"
    (Just "priority_status")
    "example_priority_status"

testExampleStatusPriority :: TestTree
testExampleStatusPriority =
  testGoldenExample
    "Sorted by status_priority"
    (Just "status_priority")
    "example_status_priority"

failureTests :: TestTree
failureTests =
  testGroup
    "Failures"
    [ testIdDupsFails,
      testIdEmptyFails,
      testIdCommaFails,
      testStatusBlockedBadRefFails,
      testStatusBadFails,
      testStatusBlockedEmptyFails,
      testStatusBlockedIdsEmptyFails
    ]

testIdDupsFails :: TestTree
testIdDupsFails =
  testGolden
    (runException @DuplicateIdE)
    "Duplicate id fails"
    "id_dups"

testIdEmptyFails :: TestTree
testIdEmptyFails =
  testGolden
    (runException @AesonException)
    "Empty id fails"
    "id_empty"

testIdCommaFails :: TestTree
testIdCommaFails =
  testGolden
    (runException @AesonException)
    "Id with comma fails"
    "id_comma"

testStatusBlockedBadRefFails :: TestTree
testStatusBlockedBadRefFails =
  testGolden
    (runException @BlockedIdRefE)
    "Status blocked non-extant id reference fails"
    "status_blocked_bad_ref"

testStatusBadFails :: TestTree
testStatusBadFails =
  testGolden
    (runException @AesonException)
    "Bad status fails"
    "status_bad"

testStatusBlockedEmptyFails :: TestTree
testStatusBlockedEmptyFails =
  testGolden
    (runException @AesonException)
    "Blocked empty status fails"
    "status_blocked_empty"

testStatusBlockedIdsEmptyFails :: TestTree
testStatusBlockedIdsEmptyFails =
  testGolden
    (runException @AesonException)
    "Blocked ids empty status fails"
    "status_blocked_ids_empty"

testGoldenExample :: TestName -> Maybe String -> FilePath -> TestTree
testGoldenExample desc mSortArg goldenFilenName = goldenVsString desc path $ do
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
        ++ sortArgs
    sortArgs = maybe [] (\a -> ["--sort", a]) mSortArg

    path = outputDir `cfp` goldenFilenName <> ".golden"

testGolden :: (List String -> IO Text) -> TestName -> FilePath -> TestTree
testGolden runner desc fileName = goldenVsString desc path $ do
  result <- runner args
  pure $ toBSL result
  where
    args =
      [ "--path",
        inputDir `cfp` fileName <> ".json",
        "list",
        "--color",
        "off"
      ]

    path = outputDir `cfp` fileName <> ".golden"

inputDir :: FilePath
inputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "List" `cfp` "input"

outputDir :: FilePath
outputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "List" `cfp` "output"
