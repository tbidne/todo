module Main (main) where

import Integration.Failures qualified as Failures
import Integration.Misc qualified as Misc
import Integration.Prelude

main :: IO ()
main =
  defaultMain
    $ testGroup
      "Integration tests"
      [ Failures.tests,
        Misc.tests
      ]
