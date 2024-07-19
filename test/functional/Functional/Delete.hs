{-# LANGUAGE QuasiQuotes #-}

module Functional.Delete (tests) where

import Functional.Prelude
import Todo.Index (DeleteE)

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Delete"
    [ testNonInteractive testEnv,
      testInteractive testEnv
    ]

testNonInteractive :: IO TestEnv -> TestTree
testNonInteractive testEnv =
  testGroup
    "Non-interactive"
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
  testGoldenRunnerParams
    (set' #runner (Just runner) params)
  where
    runner = runTodoResponses ["n"]
    params =
      mkGoldenParams
        "Does not delete a task"
        [osp|testDeleteNone|]
        ["walk_dog"]

testDeleteOne :: IO TestEnv -> TestTree
testDeleteOne =
  testGoldenRunnerParams
    $ mkGoldenParams "Deletes a task" [osp|testDeleteOne|] ["walk_dog"]

testDeleteGroup :: IO TestEnv -> TestTree
testDeleteGroup =
  testGoldenRunnerParams
    $ mkGoldenParams "Deletes a group" [osp|testDeleteGroup|] ["soccer_match"]

testDeleteMany :: IO TestEnv -> TestTree
testDeleteMany =
  testGoldenRunnerParams
    $ mkGoldenParams
      "Deletes several task"
      [osp|testDeleteMany|]
      ["haircut", "cleats", "bananas"]

testDeleteGroupSubtask :: IO TestEnv -> TestTree
testDeleteGroupSubtask =
  testGoldenRunnerParams
    $ mkGoldenParams
      "Deletes a group subtask"
      [osp|testDeleteGroupSubtask|]
      ["cleats"]

testDeleteBadId :: IO TestEnv -> TestTree
testDeleteBadId =
  testGoldenRunnerParams
    $ mkErrorGoldenParams
      "Delete non-extant id fails"
      [osp|testDeleteBadId|]
      ["bad_id"]

testDeleteBlockedId :: IO TestEnv -> TestTree
testDeleteBlockedId =
  testGoldenRunnerParams
    $ mkErrorGoldenParams
      "Delete task that blocks others fails"
      [osp|testDeleteBlockedId|]
      ["fix_car"]

testDeleteBlockedSuccess :: IO TestEnv -> TestTree
testDeleteBlockedSuccess =
  testGoldenRunnerParams (set' #indexPath (Just indexPath) params)
  where
    indexPath = inputOsPath </> [osp|delete_blocked_success.json|]

    params =
      mkGoldenParams
        "Delete task that blocks deleted tasks succeeds"
        [osp|testDeleteBlockedSuccess|]
        ["g", "t2"]

testDeleteTaskRepeatedly :: IO TestEnv -> TestTree
testDeleteTaskRepeatedly =
  testGoldenRunnerParams (set' #indexPath (Just indexPath) params)
  where
    indexPath = inputOsPath </> [osp|delete_repeatedly.json|]

    params =
      mkGoldenParams
        "Deletes tasks multiple times"
        [osp|testDeleteTaskRepeatedly|]
        ["g2", "s212", "t2"]

testInteractive :: IO TestEnv -> TestTree
testInteractive testEnv =
  testGroup
    "Interactive"
    [ testInteractiveDelete testEnv,
      testInteractiveDeleteRetry testEnv
    ]

testInteractiveDelete :: IO TestEnv -> TestTree
testInteractiveDelete =
  testGoldenRunnerParams
    $ set' #args args
    . set' #runner (Just runner)
    $ params
  where
    params =
      mkGoldenParams
        "Deletes interactively"
        [osp|testInteractiveDelete|]
        []

    args = ["delete"]

    runner = runTodoResponses responses
    responses =
      [ "haircut empty_group soccer_match",
        "y"
      ]

testInteractiveDeleteRetry :: IO TestEnv -> TestTree
testInteractiveDeleteRetry =
  testGoldenRunnerParams
    $ set' #args args
    . set' #runner (Just runner)
    $ params
  where
    params =
      mkGoldenParams
        "Deletes interactively with retry"
        [osp|testInteractiveDeleteRetry|]
        []

    args = ["delete"]

    runner = runTodoResponses responses
    responses =
      [ "",
        "bad",
        "salary",
        "haircut empty_group soccer_match",
        "y"
      ]

mkGoldenParams :: TestName -> OsPath -> List String -> GoldenParams
mkGoldenParams testDesc testDirName deleted =
  MkGoldenParams
    { indexPath = Nothing,
      runner = Just (runTodoResponses ["y"]),
      testDesc,
      dataDir = [osp|Delete|],
      testDirName,
      args,
      runList = True
    }
  where
    args = "delete" : "--interactive" : "off" : deleted

mkErrorGoldenParams :: TestName -> OsPath -> List String -> GoldenParams
mkErrorGoldenParams testDesc testDirName deleted =
  set' #runner (Just $ runTodoException @DeleteE) params
  where
    params = mkGoldenParams testDesc testDirName deleted

inputOsPath :: OsPath
inputOsPath = mkInputDir [osp|Delete|]
