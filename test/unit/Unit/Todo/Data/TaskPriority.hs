module Unit.Todo.Data.TaskPriority (tests) where

import Hedgehog.Gen qualified as Gen
import Todo.Data.TaskPriority (TaskPriority)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Unit.Todo.Data.TaskPriority"
    [ testSemigroupAssoc,
      testSemigroupCommut,
      testMonoidIdentity
    ]

testSemigroupAssoc :: TestTree
testSemigroupAssoc = testPropertyNamed desc "testSemigroupAssoc" $ property $ do
  x <- forAll genTaskPriority
  y <- forAll genTaskPriority
  z <- forAll genTaskPriority

  let l = x <> y
      r = y <> z

  annotateShow l
  annotateShow r

  -- (x <> y) <> z === x <> (y <> z)
  l <> z === x <> r
  where
    desc = "Semigroup is associative"

testSemigroupCommut :: TestTree
testSemigroupCommut = testPropertyNamed desc "testSemigroupCommut" $ property $ do
  x <- forAll genTaskPriority
  y <- forAll genTaskPriority

  x <> y === y <> x
  where
    desc = "Semigroup is commutative"

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

testMonoidIdentity :: TestTree
testMonoidIdentity = testPropertyNamed desc "testMonoidIdentity" $ property $ do
  x <- forAll genTaskPriority

  x <> mempty === x
  mempty <> x === x
  where
    desc = "Monoid identity is lawful"

genTaskPriority :: Gen TaskPriority
genTaskPriority = Gen.enumBounded
