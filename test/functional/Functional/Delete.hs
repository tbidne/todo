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
      testDeleteBlockedId testEnv,
      testDeleteBlockedSuccess testEnv,
      testDeleteTaskRepeatedly testEnv
    ]

testDeleteNone :: IO TestEnv -> TestTree
testDeleteNone =
  testDeleteGoldenRunner
    exampleJson
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
  testDeleteGolden
    "Deletes a group subtask"
    [osp|testDeleteGroupSubtask|]
    ["cleats"]

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

testDeleteBlockedSuccess :: IO TestEnv -> TestTree
testDeleteBlockedSuccess =
  testDeleteGoldenRunner
    indexPath
    (runTodoResponses ["y"])
    "Delete task that blocks deleted tasks succeeds"
    [osp|testDeleteBlockedSuccess|]
    ["g", "t2"]
  where
    indexPath = inputOsPath </> [osp|delete_blocked_success.json|]

testDeleteTaskRepeatedly :: IO TestEnv -> TestTree
testDeleteTaskRepeatedly =
  testDeleteGoldenRunner
    indexPath
    (runTodoResponses ["y"])
    "Deletes tasks multiple times"
    [osp|testDeleteTaskRepeatedly|]
    ["g2", "s212", "t2"]
  where
    indexPath = inputOsPath </> [osp|delete_repeatedly.json|]

testDeleteGolden :: TestName -> OsPath -> List String -> IO TestEnv -> TestTree
testDeleteGolden =
  testDeleteGoldenRunner exampleJson (runTodoResponses ["y"])

testDeleteErrorGolden :: TestName -> OsPath -> List String -> IO TestEnv -> TestTree
testDeleteErrorGolden =
  testDeleteGoldenRunner exampleJson (runTodoException @DeleteE)

testDeleteGoldenRunner ::
  OsPath ->
  (List String -> IO Text) ->
  TestName ->
  OsPath ->
  List String ->
  IO TestEnv ->
  TestTree
testDeleteGoldenRunner indexPath runner desc name taskIds testEnv =
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
    copyFileWithMetadata indexPath newPath

    -- run delete
    deleteResult <- runner deleteArgs

    writeActualFile actualPath deleteResult
  where
    path = outputDir `cfp` unsafeDecodeOsToFp name
    actualPath = path <> ".actual"
    goldenPath = path <> ".golden"

getTestDir' :: IO TestEnv -> OsPath -> IO OsPath
getTestDir' testEnv name = getTestDir testEnv ([osp|delete|] </> name)

exampleJson :: OsPath
exampleJson = [osp|examples|] </> [osp|index.json|]

inputOsPath :: OsPath
inputOsPath = unsafeEncodeFpToOs inputFilePath

inputFilePath :: FilePath
inputFilePath = "test" `cfp` "functional" `cfp` "Functional" `cfp` "Delete" `cfp` "input"

outputDir :: FilePath
outputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "Delete" `cfp` "output"
