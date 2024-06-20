{-# LANGUAGE QuasiQuotes #-}

module Unit.Prelude
  ( module X,
    examplePath,
  )
where

import Hedgehog as X
  ( Gen,
    PropertyName,
    annotate,
    annotateShow,
    failure,
    forAll,
    property,
    (===),
  )
import Test.Tasty as X (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit as X (Assertion, assertBool, assertFailure, testCase, (@=?))
import Test.Tasty.Hedgehog as X (testPropertyNamed)
import Todo.Prelude as X hiding (IO)
import Prelude as X (IO)

examplePath :: OsPath
examplePath = [osp|examples|] </> [osp|todo.json|]
