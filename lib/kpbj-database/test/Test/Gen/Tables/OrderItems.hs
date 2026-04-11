module Test.Gen.Tables.OrderItems where

--------------------------------------------------------------------------------

import Domain.Types.Cents (Cents (..))
import Effects.Database.Tables.OrderItems qualified as OrderItems
import Effects.Database.Tables.Orders qualified as Orders
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.Text (genText)

--------------------------------------------------------------------------------

orderItemInsertGen ::
  (MonadGen m) =>
  Orders.Id ->
  Products.Id ->
  Maybe ProductVariants.Id ->
  m OrderItems.Insert
orderItemInsertGen orderId productId variantId = do
  iiProductName <- genText
  iiVariantLabel <- Gen.text (Range.linear 0 30) Gen.alphaNum
  iiQuantity <- Gen.int64 (Range.linear 1 10)
  iiUnitPriceCents <- Cents <$> Gen.int64 (Range.linear 100 10000)
  pure
    OrderItems.Insert
      { iiOrderId = orderId,
        iiProductId = productId,
        iiVariantId = variantId,
        iiProductName = iiProductName,
        iiVariantLabel = iiVariantLabel,
        iiQuantity = iiQuantity,
        iiUnitPriceCents = iiUnitPriceCents
      }
