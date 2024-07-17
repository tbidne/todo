{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Functional.Update (tests) where

import Functional.Prelude
import Todo.Command.Update (FoundGroupNotSingleE)
import Todo.Index (BlockedIdRefE, DuplicateIdE)

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Update"
    [ deadlineTests testEnv,
      descriptionTests testEnv,
      idTests testEnv,
      priorityTests testEnv,
      statusTests testEnv
    ]

deadlineTests :: IO TestEnv -> TestTree
deadlineTests testEnv =
  testGroup
    "Deadline"
    [ testSetDeadline testEnv,
      testSetDeadlineFailure testEnv
    ]

testSetDeadline :: IO TestEnv -> TestTree
testSetDeadline =
  testUpdateGolden
    "Sets a task deadline"
    [osp|testSetDeadline|]
    ["set-deadline", "--task-id", "pack_bananas", "2020-10-15"]

testSetDeadlineFailure :: IO TestEnv -> TestTree
testSetDeadlineFailure =
  testUpdateErrorGolden
    @FoundGroupNotSingleE
    "Sets a task group deadline fails"
    [osp|testSetDeadlineFailure|]
    ["set-deadline", "--task-id", "empty_group", "2020-10-15"]

descriptionTests :: IO TestEnv -> TestTree
descriptionTests testEnv =
  testGroup
    "Description"
    [ testSetDesc testEnv,
      testSetDescGroupFailure testEnv
    ]

testSetDesc :: IO TestEnv -> TestTree
testSetDesc =
  testUpdateGolden
    "Sets a task description"
    [osp|testSetDesc|]
    ["set-description", "--task-id", "ball", "Acquire a ball."]

testSetDescGroupFailure :: IO TestEnv -> TestTree
testSetDescGroupFailure =
  testUpdateErrorGolden
    @FoundGroupNotSingleE
    "Sets a task group description fails"
    [osp|testSetDescGroupFailure|]
    ["set-description", "--task-id", "equipment", "Acquire a ball."]

idTests :: IO TestEnv -> TestTree
idTests testEnv =
  testGroup
    "Id"
    [ testSetId testEnv,
      testSetBlockingId testEnv,
      testSetNestedBlockingId testEnv,
      testSetIdDuplicateFailure testEnv
    ]

testSetId :: IO TestEnv -> TestTree
testSetId =
  testUpdateGolden
    "Sets a task id"
    [osp|testSetId|]
    ["set-id", "--task-id", "cleats", "boots"]

testSetBlockingId :: IO TestEnv -> TestTree
testSetBlockingId =
  testUpdateGolden
    "Sets a blocking task id succeeds"
    [osp|testSetBlockingId|]
    ["set-id", "--task-id", "paycheck", "salary"]

-- Tests that we correctly update a nested task that references an updated
-- blocking id (i.e. that our traversal is not shallow).
testSetNestedBlockingId :: IO TestEnv -> TestTree
testSetNestedBlockingId =
  testUpdateGoldenRunner
    indexPath
    runTodo
    "Sets a nested blocking task id succeeds"
    [osp|testSetNestedBlockingId|]
    ["set-id", "--task-id", "g1_t1", "g1_t1'"]
  where
    indexPath = inputOsPath </> [osp|set_nested_blocking_id.json|]

testSetIdDuplicateFailure :: IO TestEnv -> TestTree
testSetIdDuplicateFailure =
  testUpdateErrorGolden
    @DuplicateIdE
    "Sets a duplicate task id fails"
    [osp|testSetIdDuplicateFailure|]
    ["set-id", "--task-id", "cleats", "paycheck"]

priorityTests :: IO TestEnv -> TestTree
priorityTests testEnv =
  testGroup
    "Priority"
    [ testSetPriority testEnv
    ]

testSetPriority :: IO TestEnv -> TestTree
testSetPriority =
  testUpdateGolden
    "Sets a task priority"
    [osp|testSetPriority|]
    ["set-priority", "--task-id", "soccer_match", "low"]

statusTests :: IO TestEnv -> TestTree
statusTests testEnv =
  testGroup
    "Status"
    [ testSetStatus testEnv,
      testSetStatusBlockedSuccess testEnv,
      testSetStatusBlockedFailure testEnv
    ]

testSetStatus :: IO TestEnv -> TestTree
testSetStatus =
  testUpdateGolden
    "Sets a task status"
    [osp|testSetStatus|]
    ["set-status", "--task-id", "apples", "completed"]

testSetStatusBlockedSuccess :: IO TestEnv -> TestTree
testSetStatusBlockedSuccess =
  testUpdateGolden
    "Sets a task status to blocked text"
    [osp|testSetStatusBlockedSuccess|]
    ["set-status", "--task-id", "apples", "blocked: a blocker, <ball>, another, <fix_car>"]

testSetStatusBlockedFailure :: IO TestEnv -> TestTree
testSetStatusBlockedFailure =
  testUpdateErrorGolden
    @BlockedIdRefE
    "Sets a task status to blocked text"
    [osp|testSetStatusBlockedFailure|]
    ["set-status", "--task-id", "apples", "blocked: a blocker, <bad>, another, <fix_car>"]

testUpdateGolden :: TestName -> OsPath -> List String -> IO TestEnv -> TestTree
testUpdateGolden =
  testUpdateGoldenRunner exampleJsonOsPath runTodo

testUpdateErrorGolden ::
  forall e.
  (Exception e) =>
  TestName ->
  OsPath ->
  List String ->
  IO TestEnv ->
  TestTree
testUpdateErrorGolden =
  testUpdateGoldenRunner exampleJsonOsPath (runTodoException @e)

testUpdateGoldenRunner ::
  OsPath ->
  (List String -> IO Text) ->
  TestName ->
  OsPath ->
  List String ->
  IO TestEnv ->
  TestTree
testUpdateGoldenRunner indexPath runner desc name args testEnv =
  goldenVsFile desc goldenPath actualPath $ do
    testDir <- getTestDir' testEnv name
    let newPath = testDir </> [osp|index.json|]
        updateArgs =
          [ "--index-path",
            unsafeDecodeOsToFp newPath,
            "--color",
            "off"
          ]
            ++ args

    -- copy example to test dir
    copyFileWithMetadata indexPath newPath

    -- run update
    updateResult <- runner updateArgs

    let listArgs =
          [ "--index-path",
            unsafeDecodeOsToFp newPath,
            "--color",
            "off",
            "list"
          ]

    -- run list
    listResult <- runTodo listArgs

    writeActualFile actualPath (updateResult <> "\n\n" <> listResult)
  where
    path = outputDir `cfp` unsafeDecodeOsToFp name
    actualPath = path <> ".actual"
    goldenPath = path <> ".golden"

getTestDir' :: IO TestEnv -> OsPath -> IO OsPath
getTestDir' testEnv name = getTestDir testEnv ([osp|update|] </> name)

inputOsPath :: OsPath
inputOsPath = unsafeEncodeFpToOs inputFilePath

inputFilePath :: FilePath
inputFilePath = "test" `cfp` "functional" `cfp` "Functional" `cfp` "Update" `cfp` "input"

outputDir :: FilePath
outputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "Update" `cfp` "output"
