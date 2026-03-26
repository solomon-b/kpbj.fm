module Test.Gen.Tables.ProductOptionTypes where

--------------------------------------------------------------------------------

import Effects.Database.Tables.ProductOptionTypes qualified as ProductOptionTypes
import Effects.Database.Tables.Products qualified as Products
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.Text (genText)

--------------------------------------------------------------------------------

productOptionTypeInsertGen :: (MonadGen m) => Products.Id -> m ProductOptionTypes.Insert
productOptionTypeInsertGen productId = do
  otiName <- genText
  otiSortOrder <- Gen.int64 (Range.linear 0 100)
  pure
    ProductOptionTypes.Insert
      { otiProductId = productId,
        otiName = otiName,
        otiSortOrder = otiSortOrder
      }
