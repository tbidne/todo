module Unit.Todo.Data.TaskStatus (tests) where

import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as T
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Todo.Data.TaskId.Internal (TaskId (UnsafeTaskId))
import Todo.Data.TaskStatus
  ( Blocker (BlockerId, BlockerText),
    StatusMatch (StatusMatchSuccess),
    TaskStatus
      ( Blocked,
        Completed,
        InProgress,
        NotStarted
      ),
  )
import Todo.Data.TaskStatus qualified as Status
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Unit.Todo.Data.TaskStatus"
    [ testSemigroupAssoc,
      testSemigroupCommut,
      testStatusIsoReviewView,
      testStatusIsoViewReview,
      testParseSuccess
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

testStatusIsoReviewView :: TestTree
testStatusIsoReviewView = testPropertyNamed desc "testStatusIsoReviewView" $ property $ do
  status <- forAll genTaskStatus

  let statusMatch = StatusMatchSuccess status
      txt = view Status.statusIso statusMatch
      result = review Status.statusIso txt

  annotate $ unpack txt

  statusMatch === result
  where
    desc = "statusIso: review . view === id"

testStatusIsoViewReview :: TestTree
testStatusIsoViewReview = testPropertyNamed desc "testStatusIsoViewReview" $ property $ do
  statusTxt <- forAll getStatusText

  let statusMatch = review Status.statusIso statusTxt
      result = view Status.statusIso statusMatch

  annotateShow statusMatch

  ordSafeEq statusTxt result
  where
    desc = "statusIso: view . review === id for good text"

    -- Blockers can have their order rearranged due to the Set.
    ordSafeEq expected result =
      case liftA2 (,) (stripBlocked expected) (stripBlocked result) of
        Nothing -> expected === result
        Just (expectedRest, resultRest) -> do
          let expecteds = collectCommas expectedRest
              results = collectCommas resultRest
          expecteds === results

    stripBlocked = T.stripPrefix "blocked: "
    collectCommas = Set.fromList . fmap T.strip . T.split (== ',')

testParseSuccess :: TestTree
testParseSuccess = testPropertyNamed desc "testParseSuccess" $ property $ do
  statusText <- forAll getStatusText

  let eParseResult = Status.parseTaskStatus statusText

  case eParseResult of
    EitherLeft err -> do
      annotate err
      failure
    EitherRight _ -> pure ()
  where
    desc = "Parses expected status"

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
    [ BlockerText <$> genGoodText,
      BlockerId <$> genTaskId
    ]
  where
    genTaskId = UnsafeTaskId <$> genGoodText

getStatusText :: Gen Text
getStatusText =
  Gen.choice
    [ pure "completed",
      pure "in-progress",
      pure "not-started",
      genBlockerText
    ]

genBlockerText :: Gen Text
genBlockerText = do
  ts <- Gen.list (Range.linear 1 10) genT

  let blockers = T.intercalate ", " ts

  pure $ "blocked: " <> blockers
  where
    genT =
      Gen.choice
        [ genBlockerIdText,
          genBlockerTextText
        ]

genBlockerIdText :: Gen Text
genBlockerIdText = (\t -> "<" <> t <> ">") <$> genGoodText

genBlockerTextText :: Gen Text
genBlockerTextText = genGoodText

genGoodText :: Gen Text
genGoodText =
  Gen.filter (not . T.null)
    . fmap T.strip
    $ Gen.text (Range.linear 1 20) genGoodChar

genGoodChar :: Gen Char
genGoodChar = Gen.filter (not . flip Set.member badChars) Gen.unicode

badChars :: Set Char
badChars = Set.fromList [',', '<', '>']
