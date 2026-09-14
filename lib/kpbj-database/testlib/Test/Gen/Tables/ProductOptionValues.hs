module Test.Gen.Tables.ProductOptionValues where

--------------------------------------------------------------------------------

import Effects.Database.Tables.ProductOptionTypes qualified as ProductOptionTypes
import Effects.Database.Tables.ProductOptionValues qualified as ProductOptionValues
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.Text (genText)

--------------------------------------------------------------------------------

productOptionValueInsertGen :: (MonadGen m) => ProductOptionTypes.Id -> m ProductOptionValues.Insert
productOptionValueInsertGen optionTypeId = do
  oviValue <- genText
  oviSortOrder <- Gen.int64 (Range.linear 0 100)
  pure
    ProductOptionValues.Insert
      { oviOptionTypeId = optionTypeId,
        oviValue = oviValue,
        oviSortOrder = oviSortOrder
      }
