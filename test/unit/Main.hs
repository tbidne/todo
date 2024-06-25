module Main (main) where

import Unit.Prelude
import Unit.Todo.Data.Task qualified
import Unit.Todo.Data.Sorted qualified
import Unit.Todo.Index qualified

main :: IO ()
main =
  defaultMain
    $ testGroup
      "Unit tests"
      [ Unit.Todo.Data.Task.tests,
        Unit.Todo.Data.Sorted.tests,
        Unit.Todo.Index.tests
      ]
