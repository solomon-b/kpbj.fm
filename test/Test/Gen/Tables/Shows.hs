module Test.Gen.Tables.Shows where

--------------------------------------------------------------------------------

import Effects.Database.Tables.Shows qualified as Shows
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
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
  siLogoUrl <- Gen.maybe genUrl
  siBannerUrl <- Gen.maybe genUrl
  siStatus <- genShowStatus
  pure Shows.Insert {..}
