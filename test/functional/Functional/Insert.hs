{-# LANGUAGE QuasiQuotes #-}

module Functional.Insert (tests) where

import Functional.Prelude

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Insert"
    [ testInsertOne testEnv
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
  insertResult <- runResponsesTodo responses insertArgs

  let listArgs =
        [ "--path",
          unsafeDecodeOsToFp newPath,
          "list",
          "--color",
          "off"
        ]

  -- run list
  listResult <- run listArgs

  pure $ toBSL $ insertResult <> "\n\n" <> listResult
  where
    name = [osp|testInsertOne|]
    desc = "Inserts a single task"
    path = outputDir `cfp` "testInsertOne"
    goldenPath = path <> ".golden"

    responses =
      [ "new_id",
        "not-started",
        "normal",
        "",
        ""
      ]

getTestDir' :: IO TestEnv -> OsPath -> IO OsPath
getTestDir' testEnv name = getTestDir testEnv ([osp|insert|] </> name)

exampleJson :: OsPath
exampleJson = [osp|examples|] </> [osp|tasks.json|]

outputDir :: FilePath
outputDir = "test" `cfp` "functional" `cfp` "Functional" `cfp` "Insert" `cfp` "output"
