{-# LANGUAGE QuasiQuotes #-}

module Functional.Insert (tests) where

import Functional.Prelude

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Insert"
    [ testInsertOne testEnv,
      testInsertGroup testEnv,
      testInsertNestedGroup testEnv,
      testFailureRetry testEnv
    ]

testInsertOne :: IO TestEnv -> TestTree
testInsertOne testEnv = goldenVsString desc goldenPath $ do
  testDir <- getTestDir' testEnv name
  let newPath = testDir </> [osp|tasks.json|]
      insertArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "--color",
          "off",
          "insert"
        ]

  -- copy example to test dir
  copyFileWithMetadata exampleJsonOsPath newPath

  -- run insert
  insertResult <- runTodoResponses responses insertArgs

  let listArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "--color",
          "off",
          "list"
        ]

  -- run list
  listResult <- runTodo listArgs

  pure $ toBSL $ insertResult <> "\n\n" <> listResult
  where
    name = [osp|testInsertOne|]
    desc = "Inserts a single task"
    path = outputDir `cfp` "testInsertOne"
    goldenPath = path <> ".golden"

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

testInsertGroup :: IO TestEnv -> TestTree
testInsertGroup testEnv = goldenVsString desc goldenPath $ do
  testDir <- getTestDir' testEnv name
  let newPath = testDir </> [osp|tasks.json|]
      insertArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "--color",
          "off",
          "insert"
        ]

  -- copy example to test dir
  copyFileWithMetadata exampleJsonOsPath newPath

  -- run insert
  insertResult <- runTodoResponses responses insertArgs

  let listArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "--color",
          "off",
          "list"
        ]

  -- run list
  listResult <- runTodo listArgs

  pure $ toBSL $ insertResult <> "\n\n" <> listResult
  where
    name = [osp|testInsertGroup|]
    desc = "Inserts a task group"
    path = outputDir `cfp` "testInsertGroup"
    goldenPath = path <> ".golden"

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
testInsertNestedGroup testEnv = goldenVsString desc goldenPath $ do
  testDir <- getTestDir' testEnv name
  let newPath = testDir </> [osp|tasks.json|]
      insertArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "--color",
          "off",
          "insert"
        ]

  -- copy example to test dir
  copyFileWithMetadata exampleJsonOsPath newPath

  -- run insert
  insertResult <- runTodoResponses responses insertArgs

  let listArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "--color",
          "off",
          "list"
        ]

  -- run list
  listResult <- runTodo listArgs

  pure $ toBSL $ insertResult <> "\n\n" <> listResult
  where
    name = [osp|testInsertNestedGroup|]
    desc = "Inserts a task group"
    path = outputDir `cfp` "testInsertNestedGroup"
    goldenPath = path <> ".golden"

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

testFailureRetry :: IO TestEnv -> TestTree
testFailureRetry testEnv = goldenVsString desc goldenPath $ do
  testDir <- getTestDir' testEnv name
  let newPath = testDir </> [osp|tasks.json|]
      insertArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "--color",
          "off",
          "insert"
        ]

  -- copy example to test dir
  copyFileWithMetadata exampleJsonOsPath newPath

  -- run insert
  insertResult <- runTodoResponses responses insertArgs

  let listArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "--color",
          "off",
          "list"
        ]

  -- run list
  listResult <- runTodo listArgs

  pure $ toBSL $ insertResult <> "\n\n" <> listResult
  where
    name = [osp|testFailureRetry|]
    desc = "Failures invokes retries"
    path = outputDir `cfp` "testFailureRetry"
    goldenPath = path <> ".golden"

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

getTestDir' :: IO TestEnv -> OsPath -> IO OsPath
getTestDir' testEnv name = getTestDir testEnv ([osp|insert|] </> name)

outputDir :: FilePath
outputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "Insert" `cfp` "output"
