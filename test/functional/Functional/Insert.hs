{-# LANGUAGE QuasiQuotes #-}

module Functional.Insert (tests) where

import Functional.Prelude

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Insert"
    [ testInsertOne testEnv,
      testInsertGroup testEnv,
      testFailureRetry testEnv
    ]

testInsertOne :: IO TestEnv -> TestTree
testInsertOne testEnv = goldenVsString desc goldenPath $ do
  testDir <- getTestDir' testEnv name
  let newPath = testDir </> [osp|tasks.json|]
      insertArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "insert"
        ]

  -- copy example to test dir
  copyFileWithMetadata exampleJson newPath

  -- run insert
  insertResult <- runTodoResponses responses insertArgs

  let listArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "list",
          "--color",
          "off"
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
      [ "n",
        "new_id",
        "not-started",
        "normal",
        "",
        ""
      ]

testInsertGroup :: IO TestEnv -> TestTree
testInsertGroup testEnv = goldenVsString desc goldenPath $ do
  testDir <- getTestDir' testEnv name
  let newPath = testDir </> [osp|tasks.json|]
      insertArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "insert"
        ]

  -- copy example to test dir
  copyFileWithMetadata exampleJson newPath

  -- run insert
  insertResult <- runTodoResponses responses insertArgs

  let listArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "list",
          "--color",
          "off"
        ]

  -- run list
  listResult <- runTodo listArgs

  pure $ toBSL $ insertResult <> "\n\n" <> listResult
  where
    name = [osp|testInsertGroup|]
    desc = "Inserts a single task"
    path = outputDir `cfp` "testInsertGroup"
    goldenPath = path <> ".golden"

    responses =
      [ "y",
        "group_id",
        "",
        "",
        "task_a",
        "completed",
        "normal",
        "some description",
        "",
        "y",
        "task_b",
        "in-progress",
        "high",
        "",
        "2020-04-08",
        "n"
      ]

testFailureRetry :: IO TestEnv -> TestTree
testFailureRetry testEnv = goldenVsString desc goldenPath $ do
  testDir <- getTestDir' testEnv name
  let newPath = testDir </> [osp|tasks.json|]
      insertArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "insert"
        ]

  -- copy example to test dir
  copyFileWithMetadata exampleJson newPath

  -- run insert
  insertResult <- runTodoResponses responses insertArgs

  let listArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "list",
          "--color",
          "off"
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
      [ "n",
        "groceries",
        "test_id",
        "bad_status",
        "completed",
        "bad_priority",
        "high",
        "",
        "bad_deadline",
        "2020-05-09"
      ]

getTestDir' :: IO TestEnv -> OsPath -> IO OsPath
getTestDir' testEnv name = getTestDir testEnv ([osp|insert|] </> name)

exampleJson :: OsPath
exampleJson = [osp|examples|] </> [osp|tasks.json|]

outputDir :: FilePath
outputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "Insert" `cfp` "output"
