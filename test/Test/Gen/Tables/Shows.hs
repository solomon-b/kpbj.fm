module Test.Gen.Tables.Shows where

--------------------------------------------------------------------------------

import Effects.Database.Tables.Shows qualified as Shows
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.DomainTypes (genSlug)
import Test.Gen.Text (genText, genUrl)

--------------------------------------------------------------------------------

genShowStatus :: (MonadGen m) => m Shows.Status
genShowStatus = Gen.enumBounded

showInsertGen :: (MonadGen m) => m Shows.Insert
showInsertGen = do
  siTitle <- genText
  siSlug <- genSlug
  siDescription <- genText
  siGenre <- Gen.maybe genText
  siLogoUrl <- Gen.maybe genUrl
  siBannerUrl <- Gen.maybe genUrl
  siStatus <- genShowStatus
  siDurationMinutes <- Gen.maybe $ Gen.integral (Range.linear 15 180)
  pure Shows.Insert {..}
