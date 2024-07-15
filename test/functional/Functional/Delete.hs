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
      testDeleteMany testEnv,
      testDeleteGroupSubtask testEnv,
      testDeleteBadId testEnv,
      testDeleteBlockedId testEnv
    ]

testDeleteNone :: IO TestEnv -> TestTree
testDeleteNone =
  testDeleteGoldenRunner
    (runTodoResponses ["n"])
    "Does not delete a task"
    [osp|testDeleteNone|]
    ["walk_dog"]

testDeleteOne :: IO TestEnv -> TestTree
testDeleteOne =
  testDeleteGolden "Deletes a task" [osp|testDeleteOne|] ["walk_dog"]

testDeleteGroup :: IO TestEnv -> TestTree
testDeleteGroup =
  testDeleteGolden "Deletes a group" [osp|testDeleteGroup|] ["soccer_match"]

testDeleteMany :: IO TestEnv -> TestTree
testDeleteMany =
  testDeleteGolden
    "Deletes several task"
    [osp|testDeleteMany|]
    ["haircut", "cleats", "bananas"]

testDeleteGroupSubtask :: IO TestEnv -> TestTree
testDeleteGroupSubtask =
  testDeleteGolden "Deletes a group subtask" [osp|testDeleteGroupSubtask|] ["cleats"]

testDeleteBadId :: IO TestEnv -> TestTree
testDeleteBadId =
  testDeleteErrorGolden
    "Delete non-extant id fails"
    [osp|testDeleteBadId|]
    ["bad_id"]

testDeleteBlockedId :: IO TestEnv -> TestTree
testDeleteBlockedId =
  testDeleteErrorGolden
    "Delete task that blocks others fails"
    [osp|testDeleteBlockedId|]
    ["fix_car"]

testDeleteGolden :: TestName -> OsPath -> List String -> IO TestEnv -> TestTree
testDeleteGolden = testDeleteGoldenRunner (runTodoResponses ["y"])

testDeleteErrorGolden :: TestName -> OsPath -> List String -> IO TestEnv -> TestTree
testDeleteErrorGolden = testDeleteGoldenRunner (runTodoException @DeleteE)

testDeleteGoldenRunner ::
  (List String -> IO Text) ->
  TestName ->
  OsPath ->
  List String ->
  IO TestEnv ->
  TestTree
testDeleteGoldenRunner runner desc name taskIds testEnv =
  goldenVsFile desc goldenPath actualPath $ do
    testDir <- getTestDir' testEnv name
    let newPath = testDir </> [osp|index.json|]
        deleteArgs =
          [ "--index-path",
            unsafeDecodeOsToFp newPath,
            "--color",
            "off",
            "delete"
          ]
            ++ taskIds

    -- copy example to test dir
    copyFileWithMetadata exampleJson newPath

    -- run delete
    deleteResult <- runner deleteArgs

    let listArgs =
          [ "--index-path",
            unsafeDecodeOsToFp newPath,
            "--color",
            "off",
            "list"
          ]

    -- run list
    listResult <- runTodo listArgs

    writeActualFile actualPath (deleteResult <> "\n\n" <> listResult)
  where
    path = outputDir `cfp` unsafeDecodeOsToFp name
    actualPath = path <> ".actual"
    goldenPath = path <> ".golden"

getTestDir' :: IO TestEnv -> OsPath -> IO OsPath
getTestDir' testEnv name = getTestDir testEnv ([osp|delete|] </> name)

exampleJson :: OsPath
exampleJson = [osp|examples|] </> [osp|index.json|]

outputDir :: FilePath
outputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "Delete" `cfp` "output"
