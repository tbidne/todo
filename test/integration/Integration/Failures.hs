{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Integration.Failures (tests) where

import Control.Exception (IOException)
import Data.Text qualified as T
import Integration.Prelude
import Todo.Exception (ConfigNotFoundE, IndexNameLookupE, XdgIndexNotFoundE)

tests :: TestTree
tests =
  testGroup
    "Failures"
    [ testBadConfigPathFailure,
      testBadIndexPathFailure,
      testBadIndexNameFailure,
      testBadXdgFailure
    ]

testBadConfigPathFailure :: TestTree
testBadConfigPathFailure = testHedgehogOne desc "testBadConfigPathFailure" $ do
  result <- liftIO $ runGetConfigException @ConfigNotFoundE args
  expected === displayException result
  where
    desc = "Bad config path fails"
    args =
      [ "--config-path",
        "bad-config.toml",
        "list"
      ]
    expected = "Config file not found: 'bad-config.toml'"

testBadIndexPathFailure :: TestTree
testBadIndexPathFailure = testHedgehogOne desc "testBadIndexPathFailure" $ do
  result <- liftIO $ runGetConfigException @IOException args
  let resultStr = displayException result

  annotate resultStr

  assert $ expected `T.isPrefixOf` pack resultStr
  where
    desc = "Bad index path fails"
    args =
      [ "--index-path",
        "bad-index.json",
        "list"
      ]
    expected = "bad-index.json: withFile: does not exist"

testBadIndexNameFailure :: TestTree
testBadIndexNameFailure = testHedgehogOne desc "testBadIndexNameFailure" $ do
  result <- liftIO $ runGetConfigException @IndexNameLookupE args
  expected === displayException result
  where
    desc = "Bad index name fails"
    args =
      [ "--config-path",
        noPathConfigFilePath,
        "--index-name",
        "bad-name",
        "list"
      ]
    expected =
      mconcat
        [ "No index with name 'bad-name' found in index-legend: '",
          noPathConfigFilePath,
          "'"
        ]

testBadXdgFailure :: TestTree
testBadXdgFailure = testHedgehogOne desc "testBadXdgFailure" $ do
  result <- liftIO $ runXdgGetConfigException @XdgIndexNotFoundE badXdg args
  expected === displayException result
  where
    desc = "Xdg fallback fails"
    badXdg = [osp|non-extant-xdg|]
    args = ["list"]
    expected =
      mconcat
        [ "No index name or path was given, so we fell back to XDG config '",
          "non-extant-xdg" `cfp` "todo" `cfp` "index.json",
          "', but none were found."
        ]
