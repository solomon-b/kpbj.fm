module Store.Checkout.EmailsSpec (spec) where

--------------------------------------------------------------------------------

import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Domain.Types.Cents (Cents (..))
import Effects.Email.Send (Email (..))
import Store.Checkout.Emails
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Store.Checkout.Emails" $ do
  describe "orderConfirmationEmail" $ do
    it "sends to the buyer's email address" $
      emailTo (orderConfirmationEmail sampleOrder) `shouldBe` "buyer@example.com"

    it "sets the label to order-confirmation" $
      emailLabel (orderConfirmationEmail sampleOrder) `shouldBe` "order-confirmation"

    it "includes the order number in the subject" $
      emailSubject (orderConfirmationEmail sampleOrder) `shouldSatisfy` Text.isInfixOf "KPBJ-0042"

    it "includes the item product name in the body" $
      emailBody (orderConfirmationEmail sampleOrder) `shouldContainLazy` "Tote Bag"

    it "includes the item variant in parentheses" $
      emailBody (orderConfirmationEmail sampleOrder) `shouldContainLazy` "(Blue)"

    it "includes quantity in the body" $
      emailBody (orderConfirmationEmail sampleOrder) `shouldContainLazy` "x2"

    it "includes the unit price in the body" $
      emailBody (orderConfirmationEmail sampleOrder) `shouldContainLazy` "$25.00"

    it "includes the subtotal" $
      emailBody (orderConfirmationEmail sampleOrder) `shouldContainLazy` "Subtotal:"

    it "includes the shipping method and cost" $
      emailBody (orderConfirmationEmail sampleOrder) `shouldContainLazy` "USPS"

    it "includes the tax" $
      emailBody (orderConfirmationEmail sampleOrder) `shouldContainLazy` "Tax:"

    it "includes the total" $
      emailBody (orderConfirmationEmail sampleOrder) `shouldContainLazy` "$58.33"

    it "includes the recipient's full name" $
      emailBody (orderConfirmationEmail sampleOrder) `shouldContainLazy` "Jane Smith"

    it "includes address line 1" $
      emailBody (orderConfirmationEmail sampleOrder) `shouldContainLazy` "123 Main St"

    it "includes address line 2 when present" $
      emailBody (orderConfirmationEmail sampleOrder) `shouldContainLazy` "Apt 4B"

    it "includes city, state, and zip" $
      emailBody (orderConfirmationEmail sampleOrder) `shouldContainLazy` "Los Angeles, CA 90210"

    it "includes the payment method note" $
      emailBody (orderConfirmationEmail sampleOrder) `shouldContainLazy` "Paid via credit card."

    it "includes the station thank-you" $
      emailBody (orderConfirmationEmail sampleOrder) `shouldContainLazy` "KPBJ 95.9FM"

    it "omits address line 2 when empty" $ do
      let order = sampleOrder {oedShippingAddressLine2 = ""}
      emailBody (orderConfirmationEmail order) `shouldNotContainLazy` "Apt"

    it "omits variant parentheses for items with no variant" $ do
      let order = sampleOrder {oedItems = [sampleItemNoVariant]}
      emailBody (orderConfirmationEmail order) `shouldNotContainLazy` "()"

  describe "staffNotificationEmail" $ do
    it "sends to the notification address, not the buyer" $
      emailTo (staffNotificationEmail "staff@kpbj.fm" sampleOrder) `shouldBe` "staff@kpbj.fm"

    it "sets the label to new-order-notification" $
      emailLabel (staffNotificationEmail "staff@kpbj.fm" sampleOrder) `shouldBe` "new-order-notification"

    it "includes the order number in the subject" $
      emailSubject (staffNotificationEmail "staff@kpbj.fm" sampleOrder) `shouldSatisfy` Text.isInfixOf "KPBJ-0042"

    it "includes the buyer's email in the body" $
      emailBody (staffNotificationEmail "staff@kpbj.fm" sampleOrder) `shouldContainLazy` "buyer@example.com"

    it "includes the item product name in the body" $
      emailBody (staffNotificationEmail "staff@kpbj.fm" sampleOrder) `shouldContainLazy` "Tote Bag"

    it "includes the total" $
      emailBody (staffNotificationEmail "staff@kpbj.fm" sampleOrder) `shouldContainLazy` "Total:"

    it "includes the shipping address" $
      emailBody (staffNotificationEmail "staff@kpbj.fm" sampleOrder) `shouldContainLazy` "123 Main St"

    it "omits variant parentheses for items with no variant" $ do
      let order = sampleOrder {oedItems = [sampleItemNoVariant]}
      emailBody (staffNotificationEmail "staff@kpbj.fm" order) `shouldNotContainLazy` "()"

  describe "shippingConfirmationEmail" $ do
    it "sends to the buyer's email address" $
      emailTo (shippingConfirmationEmail sampleShipping) `shouldBe` "buyer@example.com"

    it "sets the label to shipping-confirmation" $
      emailLabel (shippingConfirmationEmail sampleShipping) `shouldBe` "shipping-confirmation"

    it "includes the order number in the subject" $
      emailSubject (shippingConfirmationEmail sampleShipping) `shouldSatisfy` Text.isInfixOf "KPBJ-0042"

    it "includes the tracking number in the body" $
      emailBody (shippingConfirmationEmail sampleShipping) `shouldContainLazy` "9400111899223081813700"

    it "includes the carrier in the body" $
      emailBody (shippingConfirmationEmail sampleShipping) `shouldContainLazy` "USPS"

    it "includes the station thank-you" $
      emailBody (shippingConfirmationEmail sampleShipping) `shouldContainLazy` "KPBJ 95.9FM"

--------------------------------------------------------------------------------
-- Fixtures

sampleItem :: OrderEmailItem
sampleItem =
  OrderEmailItem
    { oeiProductName = "Tote Bag",
      oeiVariantLabel = "Blue",
      oeiQuantity = 2,
      oeiUnitPriceCents = Cents 2500
    }

-- | An item with no variant — parentheses should not appear in the output.
sampleItemNoVariant :: OrderEmailItem
sampleItemNoVariant =
  OrderEmailItem
    { oeiProductName = "Sticker Pack",
      oeiVariantLabel = "",
      oeiQuantity = 1,
      oeiUnitPriceCents = Cents 500
    }

sampleOrder :: OrderEmailData
sampleOrder =
  OrderEmailData
    { oedOrderNumber = "KPBJ-0042",
      oedEmail = "buyer@example.com",
      oedItems = [sampleItem],
      oedSubtotalCents = Cents 5000,
      oedShippingCents = Cents 758,
      oedShippingMethod = "USPS Priority Mail",
      oedTaxCents = Cents 75,
      oedTotalCents = Cents 5833,
      oedShippingFirstName = "Jane",
      oedShippingLastName = "Smith",
      oedShippingAddressLine1 = "123 Main St",
      oedShippingAddressLine2 = "Apt 4B",
      oedShippingCity = "Los Angeles",
      oedShippingState = "CA",
      oedShippingZip = "90210",
      oedOrderDetailUrl = "https://www.kpbj.fm/dashboard/store/orders/42",
      oedDate = "April 5, 2026"
    }

sampleShipping :: ShippingEmailData
sampleShipping =
  ShippingEmailData
    { sedOrderNumber = "KPBJ-0042",
      sedEmail = "buyer@example.com",
      sedTrackingNumber = "9400111899223081813700",
      sedCarrier = "USPS"
    }

--------------------------------------------------------------------------------
-- Helpers

shouldContainLazy :: LT.Text -> LT.Text -> Expectation
shouldContainLazy haystack needle =
  haystack `shouldSatisfy` LT.isInfixOf needle

shouldNotContainLazy :: LT.Text -> LT.Text -> Expectation
shouldNotContainLazy haystack needle =
  haystack `shouldSatisfy` (not . LT.isInfixOf needle)
