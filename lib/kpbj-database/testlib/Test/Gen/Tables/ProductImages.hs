module Test.Gen.Tables.ProductImages where

--------------------------------------------------------------------------------

import Effects.Database.Tables.ProductImages qualified as ProductImages
import Effects.Database.Tables.Products qualified as Products
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.Text (genText, genUrl)

--------------------------------------------------------------------------------

productImageInsertGen :: (MonadGen m) => Products.Id -> m ProductImages.Insert
productImageInsertGen productId = do
  iiImagePath <- genUrl
  iiAltText <- genText
  iiSortOrder <- Gen.int64 (Range.linear 0 100)
  pure
    ProductImages.Insert
      { iiProductId = productId,
        iiImagePath = iiImagePath,
        iiAltText = iiAltText,
        iiSortOrder = iiSortOrder
      }
