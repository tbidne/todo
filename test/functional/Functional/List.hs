{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Functional.List (tests) where

import Control.Exception (IOException)
import Data.Aeson (AesonException)
import Data.Text qualified as T
import Functional.Prelude
import Todo.Index (BlockedIdRefE, DuplicateIdE)

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "List"
    [ sortExampleTests,
      failureTests testEnv
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

failureTests :: IO TestEnv -> TestTree
failureTests testEnv =
  testGroup
    "Failures"
    [ testIdDupsFails,
      testIdEmptyFails,
      testIdCommaFails,
      testStatusBlockedBadRefFails,
      testStatusBadFails,
      testStatusBlockedEmptyFails,
      testStatusBlockedIdsEmptyFails,
      testStatusBlockedTextLAngleFails,
      testNonExtantPathFails testEnv
    ]

testIdDupsFails :: TestTree
testIdDupsFails =
  testGolden
    (runTodoException @DuplicateIdE)
    "Duplicate id fails"
    "id_dups"

testIdEmptyFails :: TestTree
testIdEmptyFails =
  testGolden
    (runTodoException @AesonException)
    "Empty id fails"
    "id_empty"

testIdCommaFails :: TestTree
testIdCommaFails =
  testGolden
    (runTodoException @AesonException)
    "Id with comma fails"
    "id_comma"

testStatusBlockedBadRefFails :: TestTree
testStatusBlockedBadRefFails =
  testGolden
    (runTodoException @BlockedIdRefE)
    "Status blocked non-extant id reference fails"
    "status_blocked_bad_ref"

testStatusBadFails :: TestTree
testStatusBadFails =
  testGolden
    (runTodoException @AesonException)
    "Bad status fails"
    "status_bad"

testStatusBlockedEmptyFails :: TestTree
testStatusBlockedEmptyFails =
  testGolden
    (runTodoException @AesonException)
    "Blocked empty status fails"
    "status_blocked_empty"

testStatusBlockedIdsEmptyFails :: TestTree
testStatusBlockedIdsEmptyFails =
  testGolden
    (runTodoException @AesonException)
    "Blocked ids empty status fails"
    "status_blocked_ids_empty"

testStatusBlockedTextLAngleFails :: TestTree
testStatusBlockedTextLAngleFails =
  testGolden
    (runTodoException @AesonException)
    "Blocked text with left angle bracket fails"
    "status_blocked_text_langle"

testNonExtantPathFails :: IO TestEnv -> TestTree
testNonExtantPathFails testEnv = goldenVsFile desc goldenPath actualPath $ do
  testDir <- getTestDir' testEnv name
  let newPath = testDir </> [osp|non-extant|] </> [osp|index.json|]
      args =
        [ "--index-path",
          unsafeDecodeOsToFp newPath,
          "list"
        ]

  -- run list
  result <- massagePath <$> runTodoException @IOException args

  let expectedInfix =
        mconcat
          [ "todo/functional/list/testNonExtantPathFails/non-extant/",
            "index.json: withFile: does not exist"
          ]

  -- Result includes a non-deterministic dir like:
  --
  --     /some/dirs/todo/functional/list/testNonExtantPathSucceeds/non-extant/index.json
  --
  -- We instead verify the infix expectation
  let resultFixed =
        if expectedInfix `T.isInfixOf` result
          then "..." <> expectedInfix <> "..."
          else result

  writeActualFile actualPath resultFixed
  where
    name = [osp|testNonExtantPathFails|]
    desc = "Non-extant path fails"
    path = outputDir `cfp` "testNonExtantPathFails"
    goldenPath = path <> ".golden"
    actualPath = path <> ".actual"

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
testGoldenExample extraArgs desc mSortArg goldenFilenName = goldenVsFile desc goldenPath actualPath $ do
  result <- runTodo args
  writeActualFile actualPath result
  where
    args =
      [ "--index-path",
        "examples" `cfp` "index.json",
        "--color",
        "off"
      ]
        ++ extraArgs
        ++ ["list"]
        ++ sortArgs
    sortArgs = maybe [] (\a -> ["--sort", a]) mSortArg

    actualPath = outputDir `cfp` goldenFilenName <> ".actual"
    goldenPath = outputDir `cfp` goldenFilenName <> ".golden"

testGolden :: (List String -> IO Text) -> TestName -> FilePath -> TestTree
testGolden runner desc fileName = goldenVsFile desc goldenPath actualPath $ do
  result <- runner args
  writeActualFile actualPath result
  where
    args =
      [ "--index-path",
        inputDir `cfp` fileName <> ".json",
        "--color",
        "off",
        "--unicode",
        "off",
        "list"
      ]

    actualPath = outputDir `cfp` fileName <> ".actual"
    goldenPath = outputDir `cfp` fileName <> ".golden"

inputDir :: FilePath
inputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "List" `cfp` "input"

outputDir :: FilePath
outputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "List" `cfp` "output"

getTestDir' :: IO TestEnv -> OsPath -> IO OsPath
getTestDir' testEnv name = getTestDir testEnv ([osp|list|] </> name)

massagePath :: Text -> Text
#if WINDOWS
massagePath = T.replace "\\" "/"
#else
massagePath = identity
#endif
