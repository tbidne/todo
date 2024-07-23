module Main (main) where

import Test.Cli.Config.Failures qualified as Failures
import Test.Cli.Config.Misc qualified as Misc
import Test.Cli.Config.Prelude

main :: IO ()
main =
  defaultMain
    $ testGroup
      "Cli Config tests"
      [ Failures.tests,
        Misc.tests
      ]
