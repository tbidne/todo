{-# LANGUAGE QuasiQuotes #-}

module Functional.Insert (tests) where

import Functional.Prelude

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Insert"
    [ testInsertOne testEnv,
      testInsertNone testEnv,
      testInsertGroup testEnv,
      testInsertNestedGroup testEnv,
      testInsertGroupFailureRetry testEnv,
      testFailureRetry testEnv,
      testDefaultStatusPriority testEnv
    ]

testInsertOne :: IO TestEnv -> TestTree
testInsertOne =
  testGoldenRunnerParams
    $ mkGoldenParams
      "Inserts a single task"
      [osp|testInsertOne|]
      responses
  where
    responses =
      [ "y",
        "",
        "n",
        "new_id",
        "not-started",
        "normal",
        "",
        "",
        "n"
      ]

testInsertNone :: IO TestEnv -> TestTree
testInsertNone =
  testGoldenRunnerParams
    $ mkGoldenParams
      "Does not insert a task"
      [osp|testInsertNone|]
      responses
  where
    responses = [ "n" ]

testInsertGroup :: IO TestEnv -> TestTree
testInsertGroup =
  testGoldenRunnerParams
    $ mkGoldenParams
      "Inserts a task group"
      [osp|testInsertGroup|]
      responses
  where
    responses =
      [ "y",
        "",
        "y",
        "group_id",
        "",
        "",
        "y",
        "group_id",
        "n",
        "task_a",
        "completed",
        "normal",
        "some description",
        "",
        "y",
        "group_id",
        "n",
        "task_b",
        "in-progress",
        "high",
        "",
        "2020-04-08",
        "n"
      ]

testInsertNestedGroup :: IO TestEnv -> TestTree
testInsertNestedGroup =
  testGoldenRunnerParams
    $ mkGoldenParams
      "Inserts a nested task group"
      [osp|testInsertNestedGroup|]
      responses
  where
    responses =
      [ "y",
        "equipment",
        "n",
        "water",
        "not-started",
        "high",
        "",
        "",
        "n"
      ]

testInsertGroupFailureRetry :: IO TestEnv -> TestTree
testInsertGroupFailureRetry =
  testGoldenRunnerParams
    $ mkGoldenParams
      "Inserts a single task into a group after group id errors"
      [osp|testInsertGroupFailureRetry|]
      responses
  where
    responses =
      [ "y",
        "bad-group",
        "paycheck",
        "equipment",
        "n",
        "water",
        "not-started",
        "high",
        "",
        "",
        "n"
      ]

testFailureRetry :: IO TestEnv -> TestTree
testFailureRetry =
  testGoldenRunnerParams
    $ mkGoldenParams
      "Failures invokes retries"
      [osp|testFailureRetry|]
      responses
  where
    responses =
      [ "y",
        "",
        "n",
        "groceries",
        "test_id",
        "bad_status",
        "completed",
        "bad_priority",
        "high",
        "",
        "bad_deadline",
        "2020-05-09",
        "n"
      ]

testDefaultStatusPriority :: IO TestEnv -> TestTree
testDefaultStatusPriority =
  testGoldenRunnerParams
    $ mkGoldenParams
      "Sets default status and priority"
      [osp|testDefaultStatusPriority|]
      responses
  where
    responses =
      [ "y",
        "",
        "n",
        "test_id",
        "",
        "",
        "",
        "",
        "n"
      ]

mkGoldenParams :: TestName -> OsPath -> List Text -> GoldenParams
mkGoldenParams testDesc testDirName responses =
  MkGoldenParams
    { indexPath = Nothing,
      runner = Just runner,
      testDesc,
      dataDir = [osp|Insert|],
      testDirName,
      args = ["insert"],
      runList = True
    }
  where
    runner = runTodoResponses responses
