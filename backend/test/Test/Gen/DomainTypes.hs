module Test.Gen.DomainTypes where

--------------------------------------------------------------------------------

import Domain.Types.DisplayName (DisplayName, mkDisplayNameUnsafe)
import Domain.Types.FullName (FullName, mkFullNameUnsafe)
import Domain.Types.Slug (Slug, mkSlug)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

genDisplayName :: (MonadGen m) => m DisplayName
genDisplayName = do
  name <- Gen.text (Range.linear 1 50) Gen.alpha
  pure $ mkDisplayNameUnsafe name

genFullName :: (MonadGen m) => m FullName
genFullName = do
  firstName <- Gen.text (Range.linear 1 30) Gen.alpha
  lastName <- Gen.text (Range.linear 1 30) Gen.alpha
  pure $ mkFullNameUnsafe $ firstName <> " " <> lastName

genSlug :: (MonadGen m) => m Slug
genSlug = do
  -- Generate a unique slug by combining random elements
  prefix <- Gen.text (Range.linear 3 10) Gen.lower
  suffix <- Gen.text (Range.linear 3 10) Gen.alphaNum
  -- Add a separator to make it more realistic
  pure $ mkSlug $ prefix <> "-" <> suffix
