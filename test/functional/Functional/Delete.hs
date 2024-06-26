{-# LANGUAGE QuasiQuotes #-}

module Functional.Delete (tests) where

import Functional.Prelude
import Todo.Index (DeleteE)

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Delete"
    [ testDeleteNone testEnv,
      testDeleteOne testEnv,
      testDeleteGroup testEnv,
      testDeleteGroupSubtask testEnv,
      testDeleteBadId testEnv,
      testDeleteBlockedId testEnv
    ]

testDeleteNone :: IO TestEnv -> TestTree
testDeleteNone =
  testDeleteGoldenRunner
    (runTodoResponses ["n"])
    "Deletes a single task"
    [osp|testDeleteNone|]
    "walk_dog"

testDeleteOne :: IO TestEnv -> TestTree
testDeleteOne =
  testDeleteGolden "Deletes a task" [osp|testDeleteOne|] "walk_dog"

testDeleteGroup :: IO TestEnv -> TestTree
testDeleteGroup =
  testDeleteGolden "Deletes a group" [osp|testDeleteGroup|] "soccer_match"

testDeleteGroupSubtask :: IO TestEnv -> TestTree
testDeleteGroupSubtask =
  testDeleteGolden "Deletes a group subtask" [osp|testDeleteGroupSubtask|] "cleats"

testDeleteBadId :: IO TestEnv -> TestTree
testDeleteBadId =
  testDeleteErrorGolden
    "Delete non-extant id fails"
    [osp|testDeleteBadId|]
    "bad_id"

testDeleteBlockedId :: IO TestEnv -> TestTree
testDeleteBlockedId =
  testDeleteErrorGolden
    "Delete task that blocks others fails"
    [osp|testDeleteBlockedId|]
    "fix_car"

testDeleteGolden :: TestName -> OsPath -> String -> IO TestEnv -> TestTree
testDeleteGolden = testDeleteGoldenRunner (runTodoResponses ["y"])

testDeleteErrorGolden :: TestName -> OsPath -> String -> IO TestEnv -> TestTree
testDeleteErrorGolden = testDeleteGoldenRunner (runTodoException @DeleteE)

testDeleteGoldenRunner ::
  (List String -> IO Text) ->
  TestName ->
  OsPath ->
  String ->
  IO TestEnv ->
  TestTree
testDeleteGoldenRunner runner desc name taskId testEnv = goldenVsString desc goldenPath $ do
  testDir <- getTestDir' testEnv name
  let newPath = testDir </> [osp|tasks.json|]
      deleteArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "--color",
          "off",
          "delete",
          taskId
        ]

  -- copy example to test dir
  copyFileWithMetadata exampleJson newPath

  -- run delete
  deleteResult <- runner deleteArgs

  let listArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "--color",
          "off",
          "list"
        ]

  -- run list
  listResult <- runTodo listArgs

  pure $ toBSL $ deleteResult <> "\n\n" <> listResult
  where
    path = outputDir `cfp` unsafeDecodeOsToFp name
    goldenPath = path <> ".golden"

getTestDir' :: IO TestEnv -> OsPath -> IO OsPath
getTestDir' testEnv name = getTestDir testEnv ([osp|delete|] </> name)

exampleJson :: OsPath
exampleJson = [osp|examples|] </> [osp|tasks.json|]

outputDir :: FilePath
outputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "Delete" `cfp` "output"
