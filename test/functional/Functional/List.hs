{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Functional.List (tests) where

import Control.Exception (IOException)
import Data.Aeson (AesonException)
import Data.Text qualified as T
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
      testExampleReverse,
      testExampleUnicode,
      testExamplePriority,
      testExamplePriorityUnicode,
      testExampleStatus,
      testExampleStatusUnicode,
      testExamplePriorityStatus,
      testExamplePriorityStatusUnicode,
      testExampleStatusPriority,
      testExampleStatusPriorityUnicode,
      testNestedDefaultSort
    ]

testExample :: TestTree
testExample =
  testGoldenExampleUnicodeOff
    "Default sort"
    Nothing
    [osp|example|]

testExampleReverse :: TestTree
testExampleReverse =
  testGoldenRunnerParamsNoEnv params
  where
    args = ["list", "--reverse", "on"]
    params =
      set'
        #args
        args
        (mkGoldenParams "Default sort reversed" [osp|testExampleReverse|])

testExampleUnicode :: TestTree
testExampleUnicode =
  testGoldenExampleUnicodeOn
    "Default sort (unicode)"
    Nothing
    [osp|example_unicode|]

testExamplePriority :: TestTree
testExamplePriority =
  testGoldenExampleUnicodeOff
    "Sorted by priority"
    (Just "priority")
    [osp|example_priority|]

testExamplePriorityUnicode :: TestTree
testExamplePriorityUnicode =
  testGoldenExampleUnicodeOn
    "Sorted by priority (unicode)"
    (Just "priority")
    [osp|example_priority_unicode|]

testExampleStatus :: TestTree
testExampleStatus =
  testGoldenExampleUnicodeOff
    "Sorted by status"
    (Just "status")
    [osp|example_status|]

testExampleStatusUnicode :: TestTree
testExampleStatusUnicode =
  testGoldenExampleUnicodeOn
    "Sorted by status (unicode)"
    (Just "status")
    [osp|example_status_unicode|]

testExamplePriorityStatus :: TestTree
testExamplePriorityStatus =
  testGoldenExampleUnicodeOff
    "Sorted by priority_status"
    (Just "priority_status")
    [osp|example_priority_status|]

testExamplePriorityStatusUnicode :: TestTree
testExamplePriorityStatusUnicode =
  testGoldenExampleUnicodeOn
    "Sorted by priority_status (unicode)"
    (Just "priority_status")
    [osp|example_priority_status_unicode|]

testExampleStatusPriority :: TestTree
testExampleStatusPriority =
  testGoldenExampleUnicodeOff
    "Sorted by status_priority"
    (Just "status_priority")
    [osp|example_status_priority|]

testExampleStatusPriorityUnicode :: TestTree
testExampleStatusPriorityUnicode =
  testGoldenExampleUnicodeOn
    "Sorted by status_priority (unicode)"
    (Just "status_priority")
    [osp|example_status_priority_unicode|]

testNestedDefaultSort :: TestTree
testNestedDefaultSort =
  testGoldenRunnerParamsNoEnv
    $ mkGoldenParamsIndexPath
      [osp|nested_sort|]
      "Sorts nested by default"
      [osp|testNestedDefaultSort|]

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
      testStatusBlockedIdsEmptyFails,
      testStatusBlockedTextLAngleFails,
      testNonExtantPathFails,
      testBadAesonKeyFails
    ]

testIdDupsFails :: TestTree
testIdDupsFails =
  testGoldenRunnerParamsNoEnv
    $ mkGoldenParamsIndexPathError @DuplicateIdE
      [osp|id_dups|]
      "Duplicate id fails"
      [osp|testIdDupsFails|]

testIdEmptyFails :: TestTree
testIdEmptyFails =
  testGoldenRunnerParamsNoEnv
    $ mkGoldenParamsIndexPathError @AesonException
      [osp|id_empty|]
      "Empty id fails"
      [osp|testIdEmptyFails|]

testIdCommaFails :: TestTree
testIdCommaFails =
  testGoldenRunnerParamsNoEnv
    $ mkGoldenParamsIndexPathError @AesonException
      [osp|id_comma|]
      "Id with comma fails"
      [osp|testIdCommaFails|]

testStatusBlockedBadRefFails :: TestTree
testStatusBlockedBadRefFails =
  testGoldenRunnerParamsNoEnv
    $ mkGoldenParamsIndexPathError @BlockedIdRefE
      [osp|status_blocked_bad_ref|]
      "Status blocked non-extant id reference fails"
      [osp|testStatusBlockedBadRefFails|]

testStatusBadFails :: TestTree
testStatusBadFails =
  testGoldenRunnerParamsNoEnv
    $ mkGoldenParamsIndexPathError @AesonException
      [osp|status_bad|]
      "Bad status fails"
      [osp|testStatusBadFails|]

testStatusBlockedEmptyFails :: TestTree
testStatusBlockedEmptyFails =
  testGoldenRunnerParamsNoEnv
    $ mkGoldenParamsIndexPathError @AesonException
      [osp|status_blocked_empty|]
      "Blocked empty status fails"
      [osp|testStatusBlockedEmptyFails|]

testStatusBlockedIdsEmptyFails :: TestTree
testStatusBlockedIdsEmptyFails =
  testGoldenRunnerParamsNoEnv
    $ mkGoldenParamsIndexPathError @AesonException
      [osp|status_blocked_ids_empty|]
      "Blocked ids empty status fails"
      [osp|testStatusBlockedIdsEmptyFails|]

testStatusBlockedTextLAngleFails :: TestTree
testStatusBlockedTextLAngleFails =
  testGoldenRunnerParamsNoEnv
    $ mkGoldenParamsIndexPathError @AesonException
      [osp|status_blocked_text_langle|]
      "Blocked text with left angle bracket fails"
      [osp|testStatusBlockedTextLAngleFails|]

testNonExtantPathFails :: TestTree
testNonExtantPathFails =
  testGoldenRunnerParamsNoEnv params
  where
    badIndex = [osp|non-extant|] </> [osp|index.json|]
    params =
      set' #runner (Just runner)
        $ set' #indexPath (Just badIndex)
        $ mkGoldenParams "Non-extant path fails" [osp|testNonExtantPathFails|]

    runner args = do
      result <- massagePath <$> runTodoException @IOException args

      let expectedInfix = "non-extant/index.json: withFile: does not exist"

          -- Result includes a non-deterministic dir like:
          --
          --     /some/dirs/todo/functional/list/testNonExtantPathSucceeds/non-extant/index.json
          --
          -- We instead verify the infix expectation
          resultFixed =
            if expectedInfix `T.isInfixOf` result
              then "..." <> expectedInfix <> "..."
              else result
      pure resultFixed

testBadAesonKeyFails :: TestTree
testBadAesonKeyFails =
  testGoldenRunnerParamsNoEnv
    $ mkGoldenParamsIndexPathError @AesonException
      [osp|bad_key|]
      "Bad aeson key fails"
      [osp|testBadAesonKeyFails|]

testGoldenExampleUnicodeOff :: TestName -> Maybe String -> OsPath -> TestTree
testGoldenExampleUnicodeOff = testGoldenExample extraArgs
  where
    extraArgs =
      [ "--unicode",
        "off"
      ]

testGoldenExampleUnicodeOn :: TestName -> Maybe String -> OsPath -> TestTree
testGoldenExampleUnicodeOn = testGoldenExample []

testGoldenExample :: List String -> TestName -> Maybe String -> OsPath -> TestTree
testGoldenExample extraArgs desc mSortArg testName =
  testGoldenRunnerParamsNoEnv params
  where
    sortArgs = maybe [] (\a -> ["--sort", a]) mSortArg

    args =
      extraArgs
        ++ ["list"]
        ++ sortArgs

    params = set' #args args (mkGoldenParams desc testName)

mkGoldenParamsIndexPathError :: forall e. (Exception e) => OsPath -> TestName -> OsPath -> GoldenParams
mkGoldenParamsIndexPathError indexName testDesc =
  set' #runner (Just $ runTodoException @e)
    . mkGoldenParamsIndexPath indexName testDesc

mkGoldenParamsIndexPath :: OsPath -> TestName -> OsPath -> GoldenParams
mkGoldenParamsIndexPath indexName testDesc =
  set' #indexPath (Just indexPath) . mkGoldenParams testDesc
  where
    indexPath = inputOsPath </> indexName <> [osp|.json|]

mkGoldenParams :: TestName -> OsPath -> GoldenParams
mkGoldenParams testDesc testDirName =
  MkGoldenParams
    { indexPath = Nothing,
      runner = Nothing,
      testDesc,
      dataDir = [osp|List|],
      testDirName,
      args,
      runList = False
    }
  where
    args = ["list"]

inputOsPath :: OsPath
inputOsPath = mkInputDir [osp|List|]

massagePath :: Text -> Text
#if WINDOWS
massagePath = T.replace "\\" "/"
#else
massagePath = identity
#endif
