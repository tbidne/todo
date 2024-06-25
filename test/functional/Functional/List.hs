{-# LANGUAGE QuasiQuotes #-}

module Functional.List (tests) where

import Data.Aeson (AesonException)
import Functional.Prelude
import Text.Regex.TDFA ((=~))
import Todo.Index (BlockedIdRefE, DuplicateIdE)

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "List"
    [ sortExampleTests,
      failureTests,
      miscTests testEnv
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

miscTests :: IO TestEnv -> TestTree
miscTests testEnv =
  testGroup
    "Miscellaneous"
    [ testNonExtantPathSucceeds testEnv
    ]

testNonExtantPathSucceeds :: IO TestEnv -> TestTree
testNonExtantPathSucceeds testEnv = goldenVsString desc goldenPath $ do
  testDir <- getTestDir' testEnv name
  let newPath = testDir </> [osp|non-extant|] </> [osp|tasks.json|]
      args =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "list"
        ]

  -- run list
  result <- runTodo args

  -- Result includes a non-deterministic dir like:
  --
  --     tmp/nix-shell.XXoKKb/todo/functional/list/testNonExtantPathSucceeds/non-extant/tasks.json
  --
  -- To make this test work, we replace the problem dir i.e. tmp/<dir>/todo/...
  let regex :: Text
      regex = "(.*)/tmp/(.*)/todo/(.*)"
      (_, _, _, matches) = result =~ regex :: (Text, Text, Text, List Text)
      result' = case matches of
        (prologue : _ : epilogue) -> prologue <> "/tmp/<dir>/todo/" <> mconcat epilogue
        other -> error $ "Bad format: " <> unpack (mconcat other)

  pure $ toBSL result'
  where
    name = [osp|testNonExtantPathSucceeds|]
    desc = "Non-extant path succeeds"
    path = outputDir `cfp` "testNonExtantPathSucceeds"
    goldenPath = path <> ".golden"

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
  result <- runTodo args
  pure $ toBSL result
  where
    args =
      [ "--path",
        "examples" `cfp` "tasks.json",
        "--color",
        "off"
      ]
        ++ extraArgs
        ++ ["list"]
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
        "--color",
        "off",
        "--unicode",
        "off",
        "list"
      ]

    path = outputDir `cfp` fileName <> ".golden"

inputDir :: FilePath
inputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "List" `cfp` "input"

outputDir :: FilePath
outputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "List" `cfp` "output"

getTestDir' :: IO TestEnv -> OsPath -> IO OsPath
getTestDir' testEnv name = getTestDir testEnv ([osp|list|] </> name)
