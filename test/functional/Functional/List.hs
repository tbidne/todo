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
      testExampleUnicode,
      testExamplePriority,
      testExamplePriorityUnicode,
      testExampleStatus,
      testExampleStatusUnicode,
      testExamplePriorityStatus,
      testExamplePriorityStatusUnicode,
      testExampleStatusPriority,
      testExampleStatusPriorityUnicode
    ]

testExample :: TestTree
testExample =
  testGoldenExampleUnicodeOff
    "Default sort"
    Nothing
    "example"

testExampleUnicode :: TestTree
testExampleUnicode =
  testGoldenExampleUnicodeOn
    "Default sort (unicode)"
    Nothing
    "example_unicode"

testExamplePriority :: TestTree
testExamplePriority =
  testGoldenExampleUnicodeOff
    "Sorted by priority"
    (Just "priority")
    "example_priority"

testExamplePriorityUnicode :: TestTree
testExamplePriorityUnicode =
  testGoldenExampleUnicodeOn
    "Sorted by priority (unicode)"
    (Just "priority")
    "example_priority_unicode"

testExampleStatus :: TestTree
testExampleStatus =
  testGoldenExampleUnicodeOff
    "Sorted by status"
    (Just "status")
    "example_status"

testExampleStatusUnicode :: TestTree
testExampleStatusUnicode =
  testGoldenExampleUnicodeOn
    "Sorted by status (unicode)"
    (Just "status")
    "example_status_unicode"

testExamplePriorityStatus :: TestTree
testExamplePriorityStatus =
  testGoldenExampleUnicodeOff
    "Sorted by priority_status"
    (Just "priority_status")
    "example_priority_status"

testExamplePriorityStatusUnicode :: TestTree
testExamplePriorityStatusUnicode =
  testGoldenExampleUnicodeOn
    "Sorted by priority_status (unicode)"
    (Just "priority_status")
    "example_priority_status_unicode"

testExampleStatusPriority :: TestTree
testExampleStatusPriority =
  testGoldenExampleUnicodeOff
    "Sorted by status_priority"
    (Just "status_priority")
    "example_status_priority"

testExampleStatusPriorityUnicode :: TestTree
testExampleStatusPriorityUnicode =
  testGoldenExampleUnicodeOn
    "Sorted by status_priority (unicode)"
    (Just "status_priority")
    "example_status_priority_unicode"

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

testGoldenExampleUnicodeOff :: TestName -> Maybe String -> FilePath -> TestTree
testGoldenExampleUnicodeOff = testGoldenExample extraArgs
  where
    extraArgs =
      [ "--unicode",
        "off"
      ]

testGoldenExampleUnicodeOn :: TestName -> Maybe String -> FilePath -> TestTree
testGoldenExampleUnicodeOn = testGoldenExample []

testGoldenExample :: List String -> TestName -> Maybe String -> FilePath -> TestTree
testGoldenExample extraArgs desc mSortArg goldenFilenName = goldenVsString desc path $ do
  result <- run args
  pure $ toBSL result
  where
    args =
      [ "--path",
        "examples" `cfp` "tasks.json",
        "list",
        "--color",
        "off"
      ]
        ++ sortArgs
        ++ extraArgs
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
        "off",
        "--unicode",
        "off"
      ]

    path = outputDir `cfp` fileName <> ".golden"

inputDir :: FilePath
inputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "List" `cfp` "input"

outputDir :: FilePath
outputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "List" `cfp` "output"
