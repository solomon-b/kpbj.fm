module Test.Gen.Text where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

genText :: (MonadGen m) => m Text
genText = Gen.text (Range.linear 5 100) Gen.alpha -- Use alpha instead of unicode to avoid null bytes

genShortText :: (MonadGen m) => m Text
genShortText = Gen.text (Range.linear 3 20) Gen.alphaNum

genSlug :: (MonadGen m) => m Text
genSlug = do
  prefix <- Gen.text (Range.linear 3 10) Gen.lower
  suffix <- Gen.text (Range.linear 3 10) Gen.alphaNum
  pure $ prefix <> "-" <> suffix

genUrl :: (MonadGen m) => m Text
genUrl = do
  path <- Gen.text (Range.linear 1 50) Gen.alphaNum
  pure $ "/static/uploads/" <> path
