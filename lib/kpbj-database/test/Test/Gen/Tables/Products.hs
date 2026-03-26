module Test.Gen.Tables.Products where

--------------------------------------------------------------------------------

import Domain.Types.Cents (Cents (..))
import Data.Int (Int64)
import Effects.Database.Tables.Products qualified as Products
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.Text (genSlug, genText)

--------------------------------------------------------------------------------

genCents :: (MonadGen m) => m Cents
genCents = Cents <$> Gen.int64 (Range.linear 0 100000)

genSortOrder :: (MonadGen m) => m Int64
genSortOrder = Gen.int64 (Range.linear 0 100)

genWeightOz :: (MonadGen m) => m Int64
genWeightOz = Gen.int64 (Range.linear 0 1000)

genInventoryCount :: (MonadGen m) => m Int64
genInventoryCount = Gen.int64 (Range.linear 0 10000)

productInsertGen :: (MonadGen m) => m Products.Insert
productInsertGen = do
  piName <- genText
  piSlug <- genSlug
  piDescription <- genText
  piBasePriceCents <- genCents
  piWeightOz <- genWeightOz
  piCategory <- Gen.maybe genText
  piInventoryCount <- genInventoryCount
  piIsActive <- Gen.bool
  piSortOrder <- genSortOrder
  pure Products.Insert {..}
