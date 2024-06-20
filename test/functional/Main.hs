module Main (main) where

import Functional.List qualified as List
import Functional.Prelude

main :: IO ()
main =
  defaultMain
    $ testGroup
      "Functional tests"
      [ List.tests
      ]
