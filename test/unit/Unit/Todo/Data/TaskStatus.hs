module Unit.Todo.Data.TaskStatus (tests) where

import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Todo.Data.TaskId.Internal (TaskId (UnsafeTaskId))
import Todo.Data.TaskStatus
  ( Blocker (BlockerId, BlockerText),
    TaskStatus
      ( Blocked,
        Completed,
        InProgress,
        NotStarted
      ),
  )
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Unit.Todo.Data.TaskStatus"
    [ testSemigroupAssoc,
      testSemigroupCommut
    ]

testSemigroupAssoc :: TestTree
testSemigroupAssoc = testPropertyNamed desc "testSemigroupAssoc" $ property $ do
  x <- forAll genTaskStatus
  y <- forAll genTaskStatus
  z <- forAll genTaskStatus

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
  x <- forAll genTaskStatus
  y <- forAll genTaskStatus

  x <> y === y <> x
  where
    desc = "Semigroup is commutative"

genTaskStatus :: Gen TaskStatus
genTaskStatus =
  Gen.choice
    [ pure Completed,
      pure NotStarted,
      pure InProgress,
      Blocked <$> genBlocked
    ]
  where
    genBlocked = NESet.fromList <$> genBlockedNE
    genBlockedNE = Gen.nonEmpty (Range.linear 1 20) genBlocker

    genBlocker :: Gen Blocker
    genBlocker =
      Gen.choice
        [ BlockerText <$> genText,
          BlockerId <$> genTaskId
        ]

    genTaskId = UnsafeTaskId . (\t -> "<" <> t <> ">") <$> genText

    genText = Gen.text (Range.linear 1 20) genGoodChar

    genGoodChar = Gen.filter (not . flip Set.member badChars) Gen.unicode

    badChars = Set.fromList [',', '<', '>']
