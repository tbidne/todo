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
  testGoldenRunnerParams
    $ mkGoldenParams
      "Sets a task deadline"
      [osp|testSetDeadline|]
      ["set-deadline", "--task-id", "pack_bananas", "2020-10-15"]

testSetDeadlineFailure :: IO TestEnv -> TestTree
testSetDeadlineFailure =
  testGoldenRunnerParams
    (set' #runner (Just $ runTodoException @FoundGroupNotSingleE) params)
  where
    params =
      mkGoldenParams
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
  testGoldenRunnerParams
    $ mkGoldenParams
      "Sets a task description"
      [osp|testSetDesc|]
      ["set-description", "--task-id", "ball", "Acquire a ball."]

testSetDescGroupFailure :: IO TestEnv -> TestTree
testSetDescGroupFailure =
  testGoldenRunnerParams
    (set' #runner (Just $ runTodoException @FoundGroupNotSingleE) params)
  where
    params =
      mkGoldenParams
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
  testGoldenRunnerParams
    $ mkGoldenParams
      "Sets a task id"
      [osp|testSetId|]
      ["set-id", "--task-id", "cleats", "boots"]

testSetBlockingId :: IO TestEnv -> TestTree
testSetBlockingId =
  testGoldenRunnerParams
    $ mkGoldenParams
      "Sets a blocking task id succeeds"
      [osp|testSetBlockingId|]
      ["set-id", "--task-id", "paycheck", "salary"]

-- Tests that we correctly update a nested task that references an updated
-- blocking id (i.e. that our traversal is not shallow).
testSetNestedBlockingId :: IO TestEnv -> TestTree
testSetNestedBlockingId =
  testGoldenRunnerParams
    (set' #indexPath (Just indexPath) params)
  where
    indexPath = inputOsPath </> [osp|set_nested_blocking_id.json|]
    params =
      mkGoldenParams
        "Sets a nested blocking task id succeeds"
        [osp|testSetNestedBlockingId|]
        ["set-id", "--task-id", "g1_t1", "g1_t1'"]

testSetIdDuplicateFailure :: IO TestEnv -> TestTree
testSetIdDuplicateFailure =
  testGoldenRunnerParams
    (set' #runner (Just $ runTodoException @DuplicateIdE) params)
  where
    params =
      mkGoldenParams
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
  testGoldenRunnerParams
    $ mkGoldenParams
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
  testGoldenRunnerParams
    $ mkGoldenParams
      "Sets a task status"
      [osp|testSetStatus|]
      ["set-status", "--task-id", "apples", "completed"]

testSetStatusBlockedSuccess :: IO TestEnv -> TestTree
testSetStatusBlockedSuccess =
  testGoldenRunnerParams
    $ mkGoldenParams
      "Sets a task status to blocked text"
      [osp|testSetStatusBlockedSuccess|]
      ["set-status", "--task-id", "apples", "blocked: a blocker, <ball>, another, <fix_car>"]

testSetStatusBlockedFailure :: IO TestEnv -> TestTree
testSetStatusBlockedFailure =
  testGoldenRunnerParams
    (set' #runner (Just $ runTodoException @BlockedIdRefE) params)
  where
    params =
      mkGoldenParams
        "Sets a task status to blocked text"
        [osp|testSetStatusBlockedFailure|]
        ["set-status", "--task-id", "apples", "blocked: a blocker, <bad>, another, <fix_car>"]

mkGoldenParams :: TestName -> OsPath -> List String -> GoldenParams
mkGoldenParams testDesc testDirName args =
  MkGoldenParams
    { indexPath = Nothing,
      runner = Nothing,
      testDesc,
      dataDir = [osp|Update|],
      testDirName,
      args,
      runList = True
    }

inputOsPath :: OsPath
inputOsPath = mkInputDir [osp|Update|]
