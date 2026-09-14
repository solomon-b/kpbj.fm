module Test.Gen.Tables.Orders where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Effects.Database.Tables.Orders qualified as Orders
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.DomainTypes (genCents)
import Test.Gen.Text (genShortText, genText)

--------------------------------------------------------------------------------

genOrderNumber :: (MonadGen m) => m Text
genOrderNumber = do
  suffix <- Gen.text (Range.linear 6 10) Gen.alphaNum
  pure $ "KPBJ-" <> suffix

genEmail :: (MonadGen m) => m Text
genEmail = do
  username <- Gen.text (Range.linear 3 10) Gen.alphaNum
  domain <- Gen.text (Range.linear 3 8) Gen.alpha
  pure $ username <> "@" <> domain <> ".com"

genPaymentMethod :: (MonadGen m) => m Orders.PaymentMethod
genPaymentMethod = Gen.element [Orders.Stripe, Orders.Paypal]

genOrderStatus :: (MonadGen m) => m Orders.OrderStatus
genOrderStatus = Gen.element [Orders.Pending, Orders.Paid, Orders.Shipped, Orders.Completed, Orders.Cancelled, Orders.Refunded]

orderInsertGen :: (MonadGen m) => m Orders.Insert
orderInsertGen = do
  oiOrderNumber <- genOrderNumber
  oiEmail <- genEmail
  oiShippingFirstName <- genShortText
  oiShippingLastName <- genShortText
  oiShippingAddressLine1 <- genText
  oiShippingAddressLine2 <- Gen.text (Range.linear 0 50) Gen.alphaNum
  oiShippingCity <- genShortText
  oiShippingState <- Gen.text (Range.constant 2 2) Gen.alpha
  oiShippingZip <- Gen.text (Range.constant 5 5) Gen.digit
  oiShippingCountry <- pure "US"
  oiShippingMethod <- Gen.element ["standard", "express", "priority"]
  oiSubtotalCents <- genCents
  oiShippingCents <- genCents
  oiTaxCents <- genCents
  let oiTotalCents = oiSubtotalCents + oiShippingCents + oiTaxCents
  oiPaymentMethod <- genPaymentMethod
  oiStripeCheckoutSessionId <- Gen.maybe genText
  pure Orders.Insert {..}
