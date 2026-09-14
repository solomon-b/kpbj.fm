module Test.Gen.Tables.ProductVariants where

--------------------------------------------------------------------------------

import Domain.Types.Cents (Cents (..))
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Effects.Database.Tables.Products qualified as Products
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.Text (genText, genShortText)

--------------------------------------------------------------------------------

productVariantInsertGen :: (MonadGen m) => Products.Id -> m ProductVariants.Insert
productVariantInsertGen productId = do
  viLabel <- genText
  viPriceCents <- Gen.maybe (Cents <$> Gen.int64 (Range.linear 0 100000))
  viInventoryCount <- Gen.int64 (Range.linear 0 10000)
  viSku <- Gen.maybe genShortText
  viWeightOz <- Gen.maybe (Gen.int64 (Range.linear 0 1000))
  viSortOrder <- Gen.int64 (Range.linear 0 100)
  pure
    ProductVariants.Insert
      { viProductId = productId,
        viLabel = viLabel,
        viPriceCents = viPriceCents,
        viInventoryCount = viInventoryCount,
        viSku = viSku,
        viWeightOz = viWeightOz,
        viSortOrder = viSortOrder
      }
