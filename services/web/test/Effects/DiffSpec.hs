module Effects.DiffSpec (spec) where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Diff
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------
-- Helpers

isContext :: DiffLine -> Bool
isContext (Context _) = True
isContext _ = False

isAdded :: DiffLine -> Bool
isAdded (Added _) = True
isAdded _ = False

isRemoved :: DiffLine -> Bool
isRemoved (Removed _) = True
isRemoved _ = False

countContext :: [DiffLine] -> Int
countContext = length . filter isContext

countAdded :: [DiffLine] -> Int
countAdded = length . filter isAdded

countRemoved :: [DiffLine] -> Int
countRemoved = length . filter isRemoved

--------------------------------------------------------------------------------
-- Generators

-- | Generate multiline text content (simulating page content)
genMultilineText :: (MonadGen m) => m Text
genMultilineText = do
  numLines <- Gen.int (Range.linear 0 20)
  lines' <- Gen.list (Range.singleton numLines) genLine
  pure $ Text.intercalate "\n" lines'
  where
    genLine = Gen.text (Range.linear 0 80) Gen.alphaNum

-- | Generate non-empty multiline text
genNonEmptyMultilineText :: (MonadGen m) => m Text
genNonEmptyMultilineText = do
  numLines <- Gen.int (Range.linear 1 20)
  lines' <- Gen.list (Range.singleton numLines) genLine
  pure $ Text.intercalate "\n" lines'
  where
    genLine = Gen.text (Range.linear 1 80) Gen.alphaNum

--------------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = describe "Effects.Diff" $ do
  describe "computeLineDiff" $ do
    describe "Unit Tests" $ do
      it "returns empty list for two empty strings" $ do
        computeLineDiff "" "" `shouldBe` []

      it "returns all Removed for old content diffed against empty" $ do
        let result = computeLineDiff "line1\nline2\nline3" ""
        all isRemoved result `shouldBe` True
        length result `shouldBe` 3

      it "returns all Added for empty diffed against new content" $ do
        let result = computeLineDiff "" "line1\nline2\nline3"
        all isAdded result `shouldBe` True
        length result `shouldBe` 3

      it "returns all Context for identical content" $ do
        let content = "line1\nline2\nline3"
            result = computeLineDiff content content
        all isContext result `shouldBe` True
        length result `shouldBe` 3

      it "detects single line addition" $ do
        let old = "line1\nline3"
            new = "line1\nline2\nline3"
            result = computeLineDiff old new
        countAdded result `shouldBe` 1
        countContext result `shouldBe` 2

      it "detects single line removal" $ do
        let old = "line1\nline2\nline3"
            new = "line1\nline3"
            result = computeLineDiff old new
        countRemoved result `shouldBe` 1
        countContext result `shouldBe` 2

      it "detects line modification as remove + add" $ do
        let old = "hello world"
            new = "hello universe"
            result = computeLineDiff old new
        countRemoved result `shouldBe` 1
        countAdded result `shouldBe` 1
        countContext result `shouldBe` 0

      it "handles trailing newlines correctly" $ do
        let old = "line1\nline2\n"
            new = "line1\nline2\n"
            result = computeLineDiff old new
        -- Text.lines "a\nb\n" = ["a", "b", ""]
        all isContext result `shouldBe` True

    describe "Property Tests" $ do
      it "identical text produces only Context lines" $ hedgehog $ do
        text <- forAll genMultilineText
        let result = computeLineDiff text text
        assert $ all isContext result

      it "empty old text produces only Added lines" $ hedgehog $ do
        new <- forAll genNonEmptyMultilineText
        let result = computeLineDiff "" new
        assert $ all isAdded result

      it "empty new text produces only Removed lines" $ hedgehog $ do
        old <- forAll genNonEmptyMultilineText
        let result = computeLineDiff old ""
        assert $ all isRemoved result

      it "swapping old/new swaps Added/Removed counts" $ hedgehog $ do
        old <- forAll genMultilineText
        new <- forAll genMultilineText
        let forwardDiff = computeLineDiff old new
            reverseDiff = computeLineDiff new old
        countAdded forwardDiff === countRemoved reverseDiff
        countRemoved forwardDiff === countAdded reverseDiff

      it "context count is same regardless of direction" $ hedgehog $ do
        old <- forAll genMultilineText
        new <- forAll genMultilineText
        let forwardDiff = computeLineDiff old new
            reverseDiff = computeLineDiff new old
        countContext forwardDiff === countContext reverseDiff

      it "context + removed = old line count" $ hedgehog $ do
        old <- forAll genMultilineText
        new <- forAll genMultilineText
        let diffResult = computeLineDiff old new
            oldLineCount = length (Text.lines old)
        countContext diffResult + countRemoved diffResult === oldLineCount

      it "context + added = new line count" $ hedgehog $ do
        old <- forAll genMultilineText
        new <- forAll genMultilineText
        let diffResult = computeLineDiff old new
            newLineCount = length (Text.lines new)
        countContext diffResult + countAdded diffResult === newLineCount

      it "total diff lines = context + added + removed" $ hedgehog $ do
        old <- forAll genMultilineText
        new <- forAll genMultilineText
        let diffResult = computeLineDiff old new
        length diffResult === countContext diffResult + countAdded diffResult + countRemoved diffResult
