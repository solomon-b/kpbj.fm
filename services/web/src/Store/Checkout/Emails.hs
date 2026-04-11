{-# LANGUAGE QuasiQuotes #-}

-- | Email templates for store order events.
--
-- All functions are pure: they take order data and return 'Email' values
-- ready to pass to 'Effects.Email.Send.sendAsync'.
module Store.Checkout.Emails
  ( -- * Data types
    OrderEmailData (..),
    OrderEmailItem (..),
    ShippingEmailData (..),

    -- * Email constructors
    orderConfirmationEmail,
    staffNotificationEmail,
    shippingConfirmationEmail,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Data.String.Interpolate (i)
import Domain.Types.Cents (Cents, formatDisplay)
import Effects.Email.Send (Email (..))

--------------------------------------------------------------------------------

-- | All data needed to render order-related emails to the buyer.
data OrderEmailData = OrderEmailData
  { oedOrderNumber :: Text,
    oedEmail :: Text,
    oedItems :: [OrderEmailItem],
    oedSubtotalCents :: Cents,
    oedShippingCents :: Cents,
    oedShippingMethod :: Text,
    oedTaxCents :: Cents,
    oedTotalCents :: Cents,
    oedShippingFirstName :: Text,
    oedShippingLastName :: Text,
    oedShippingAddressLine1 :: Text,
    oedShippingAddressLine2 :: Text,
    oedShippingCity :: Text,
    oedShippingState :: Text,
    oedShippingZip :: Text,
    -- | Pre-formatted date string (e.g. "April 5, 2026")
    oedDate :: Text
  }


-- | A single line item within an order email.
data OrderEmailItem = OrderEmailItem
  { oeiProductName :: Text,
    -- | Empty string if the product has no variant
    oeiVariantLabel :: Text,
    oeiQuantity :: Int64,
    oeiUnitPriceCents :: Cents
  }


-- | Data needed to render a shipping confirmation email.
data ShippingEmailData = ShippingEmailData
  { sedOrderNumber :: Text,
    sedEmail :: Text,
    sedTrackingNumber :: Text,
    sedCarrier :: Text
  }

--------------------------------------------------------------------------------

-- | Order confirmation email sent to the buyer after a successful purchase.
orderConfirmationEmail :: OrderEmailData -> Email
orderConfirmationEmail od =
  Email
    { emailTo = oedEmail od,
      emailSubject = [i|KPBJ Order Confirmed — #{oedOrderNumber od}|],
      emailBody = LT.fromStrict (renderOrderConfirmationBody od),
      emailLabel = "order-confirmation"
    }


-- | Staff notification email sent to the store admin when a new order arrives.
--
-- The first argument is the notification email address to send to.
staffNotificationEmail :: Text -> OrderEmailData -> Email
staffNotificationEmail notificationEmail od =
  Email
    { emailTo = notificationEmail,
      emailSubject = [i|New Order — #{oedOrderNumber od}|],
      emailBody = LT.fromStrict (renderStaffNotificationBody od),
      emailLabel = "new-order-notification"
    }


-- | Shipping confirmation email sent to the buyer when an order ships.
shippingConfirmationEmail :: ShippingEmailData -> Email
shippingConfirmationEmail sd =
  Email
    { emailTo = sedEmail sd,
      emailSubject = [i|Your KPBJ Order Has Shipped — #{sedOrderNumber sd}|],
      emailBody = LT.fromStrict (renderShippingConfirmationBody sd),
      emailLabel = "shipping-confirmation"
    }

--------------------------------------------------------------------------------
-- Body renderers

renderOrderConfirmationBody :: OrderEmailData -> Text
renderOrderConfirmationBody od =
  Text.unlines $
    concat
      [ [ "Your order has been placed.",
          "",
          [i|Order: #{oedOrderNumber od}|],
          [i|Date: #{oedDate od}|],
          "",
          "Items:"
        ],
        map renderItemLineWithPrice (oedItems od),
        [ "",
          [i|Subtotal: #{formatDisplay (oedSubtotalCents od)}|],
          [i|Shipping (#{oedShippingMethod od}): #{formatDisplay (oedShippingCents od)}|],
          [i|Tax: #{formatDisplay (oedTaxCents od)}|],
          [i|Total: #{formatDisplay (oedTotalCents od)}|],
          "",
          "Shipping to:"
        ],
        renderShippingAddress od,
        [ "",
          "Paid via credit card.",
          "",
          "Thank you for supporting KPBJ 95.9FM!"
        ]
      ]


renderStaffNotificationBody :: OrderEmailData -> Text
renderStaffNotificationBody od =
  Text.unlines $
    concat
      [ [ "New order received.",
          "",
          [i|Order: #{oedOrderNumber od}|],
          [i|Customer: #{oedEmail od}|],
          "",
          "Items:"
        ],
        map renderItemLineNoPrice (oedItems od),
        [ "",
          [i|Total: #{formatDisplay (oedTotalCents od)}|],
          "",
          "Shipping to:"
        ],
        renderShippingAddress od
      ]


renderShippingConfirmationBody :: ShippingEmailData -> Text
renderShippingConfirmationBody sd =
  Text.unlines
    [ "Your order is on its way!",
      "",
      [i|Order: #{sedOrderNumber sd}|],
      [i|Tracking: #{sedTrackingNumber sd}|],
      [i|Carrier: #{sedCarrier sd}|],
      "",
      "Thank you for supporting KPBJ 95.9FM!"
    ]

--------------------------------------------------------------------------------
-- Item line renderers

-- | Render an item line with unit price for the buyer confirmation email.
--
-- Items with a variant: "  Tote Bag (Blue) x2 — $25.00"
-- Items without a variant: "  Tote Bag x2 — $25.00"
renderItemLineWithPrice :: OrderEmailItem -> Text
renderItemLineWithPrice item =
  "  " <> renderItemDescription item <> " — " <> formatDisplay (oeiUnitPriceCents item)


-- | Render an item line without price for the staff notification email.
--
-- Items with a variant: "  Tote Bag (Blue) x2"
-- Items without a variant: "  Tote Bag x2"
renderItemLineNoPrice :: OrderEmailItem -> Text
renderItemLineNoPrice item =
  "  " <> renderItemDescription item


-- | Render the product name, optional variant, and quantity portion of a line.
renderItemDescription :: OrderEmailItem -> Text
renderItemDescription item =
  let nameAndVariant =
        if Text.null (oeiVariantLabel item)
          then oeiProductName item
          else [i|#{oeiProductName item} (#{oeiVariantLabel item})|]
   in [i|#{nameAndVariant} x#{oeiQuantity item}|]

--------------------------------------------------------------------------------
-- Address renderer

-- | Render the shipping address block as indented lines.
--
-- Address line 2 is omitted entirely when empty.
renderShippingAddress :: OrderEmailData -> [Text]
renderShippingAddress od =
  let fullName = [i|  #{oedShippingFirstName od} #{oedShippingLastName od}|]
      line1 = "  " <> oedShippingAddressLine1 od
      line2 = if Text.null (oedShippingAddressLine2 od)
                then []
                else ["  " <> oedShippingAddressLine2 od]
      cityStateZip = [i|  #{oedShippingCity od}, #{oedShippingState od} #{oedShippingZip od}|]
   in [fullName, line1] <> line2 <> [cityStateZip]
