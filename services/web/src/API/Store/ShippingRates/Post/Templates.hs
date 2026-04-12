{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Template for the shipping rates HTML fragment.
--
-- Returns an HTML fragment (not a full page) that is HTMX-swapped into
-- @#shipping-rates@ on the checkout page. Renders a list of rate radio
-- buttons sorted cheapest-first, a hidden shipment ID input, and an
-- order totals section with a "Proceed to Payment" button.
module API.Store.ShippingRates.Post.Templates
  ( template,
  )
where

--------------------------------------------------------------------------------

import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Scientific (Scientific)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cents (Cents (..))
import Domain.Types.Cents qualified as Cents
import EasyPost.Types (Rate (..), Shipment (..))
import Lucid qualified
import Lucid.Alpine
import Lucid.Base (makeAttributes)
import Store.Checkout.Logic (computeTax, easypostRateToCents)

--------------------------------------------------------------------------------

-- | Shipping rates fragment template.
--
-- Renders radio buttons for each rate, a hidden shipment ID input, and
-- an order totals section. The totals update via Alpine.js when the user
-- selects a rate.
template ::
  -- | The EasyPost shipment (provides shipment ID and rate list)
  Shipment ->
  -- | Rates sorted cheapest-first
  [Rate] ->
  -- | Subtotal in cents
  Cents ->
  -- | Tax rate (e.g. @0.095@ for 9.5%)
  Scientific ->
  Lucid.Html ()
template shipment rates subtotal taxRate =
  Lucid.div_
    [ xData_ (ratesAlpineState subtotal taxRate),
      class_ $ base [Tokens.border2, Tokens.borderDefault, Tokens.p4]
    ]
    $ do
      Lucid.h2_
        [class_ $ base [Tokens.headingLg, Tokens.mb4, "uppercase"]]
        "Shipping Options"

      case rates of
        [] -> renderNoRates
        _ -> do
          -- Hidden shipment ID for form submission
          Lucid.input_
            [ Lucid.type_ "hidden",
              Lucid.name_ "shipment_id",
              Lucid.value_ shipment.id
            ]

          -- Rate radio buttons
          Lucid.div_
            [class_ $ base [Tokens.mb4]]
            $ for_ rates renderRateOption

          -- Order totals
          renderTotals subtotal taxRate

          -- Buttons
          Lucid.div_ [class_ $ base [Tokens.mt4, "flex", Tokens.gap4]] $ do
            Lucid.button_
              [ Lucid.type_ "button",
                xOn_ "click" "step = 'address'",
                class_ $
                  base
                    [ Tokens.bgAlt,
                      Tokens.fgPrimary,
                      Tokens.border2,
                      Tokens.borderDefault,
                      Tokens.px4,
                      Tokens.py2,
                      Tokens.fontBold,
                      "uppercase"
                    ]
              ]
              "\x2190 Back"
            Lucid.button_
              [ Lucid.type_ "button",
                makeAttributes ":disabled" "selectedRateId === null",
                xOn_ "click" "proceedToPayment()",
                class_ $
                  base
                    [ Tokens.buttonPrimary,
                      "flex-1",
                      "uppercase",
                      "disabled:opacity-50",
                      "disabled:cursor-not-allowed"
                    ]
              ]
              "Proceed to Payment"

-- | Render a message when no rates are available.
renderNoRates :: Lucid.Html ()
renderNoRates =
  Lucid.p_
    [class_ $ base [Tokens.textSm, Tokens.fgMuted]]
    "No shipping options are available for this address."

-- | Render a single rate radio button option.
renderRateOption :: Rate -> Lucid.Html ()
renderRateOption r =
  Lucid.label_
    [ class_ $
        base
          [ "flex",
            "items-center",
            "justify-between",
            Tokens.border2,
            Tokens.borderDefault,
            Tokens.p4,
            Tokens.mb2,
            "cursor-pointer"
          ]
    ]
    $ do
      Lucid.div_
        [class_ $ base ["flex", "items-center", Tokens.gap4]]
        $ do
          Lucid.input_
            [ Lucid.type_ "radio",
              Lucid.name_ "shipping_rate",
              Lucid.value_ rateId,
              makeAttributes "data-price" priceAttr,
              xOn_ "change" [i|selectedRateId = '#{rateId}'; selectedRatePrice = #{priceAttr}|],
              class_ $ base ["cursor-pointer"]
            ]
          Lucid.div_ $ do
            Lucid.span_
              [class_ $ base [Tokens.fontBold, Tokens.fgPrimary]]
              $ Lucid.toHtml (r.carrier <> " " <> r.service)
            case r.deliveryDays of
              Nothing -> mempty
              Just days ->
                Lucid.span_
                  [class_ $ base [Tokens.textSm, Tokens.fgMuted, "ml-2"]]
                  $ Lucid.toHtml (deliveryDaysLabel days)
      Lucid.span_
        [class_ $ base [Tokens.fontBold, Tokens.fgPrimary]]
        $ Lucid.toHtml displayPrice
  where
    rateId :: Text
    rateId = r.id

    priceAttr :: Text
    priceAttr = case easypostRateToCents r.rate of
      Just (Cents c) -> Text.pack (show c)
      Nothing -> "0"

    displayPrice :: Text
    displayPrice =
      maybe ("$" <> r.rate) Cents.formatDisplay (easypostRateToCents r.rate)

    deliveryDaysLabel :: Int -> Text
    deliveryDaysLabel days =
      "(" <> Text.pack (show days) <> " day" <> (if days == 1 then "" else "s") <> ")"

-- | Render the order totals section.
--
-- Uses Alpine.js @x-text@ bindings so subtotal, shipping and total update
-- reactively when the user selects a rate.
renderTotals ::
  Cents ->
  Scientific ->
  Lucid.Html ()
renderTotals subtotal taxRate =
  Lucid.div_
    [class_ $ base ["border-t", Tokens.borderDefault, Tokens.py2]]
    $ do
      renderTotalLine "Subtotal" (Cents.formatDisplay subtotal)
      Lucid.div_
        [class_ $ base ["flex", "justify-between", Tokens.textSm, Tokens.py2]]
        $ do
          Lucid.span_
            [class_ $ base [Tokens.fgMuted]]
            "Shipping"
          Lucid.span_
            [ class_ $ base [Tokens.fgPrimary],
              xText_ "selectedRatePrice === null ? '—' : formatCents(selectedRatePrice)"
            ]
            mempty
      renderTotalLine "Tax" (Cents.formatDisplay taxAmount)
      Lucid.div_
        [class_ $ base ["flex", "justify-between", Tokens.textSm, Tokens.py2, Tokens.fontBold]]
        $ do
          Lucid.span_
            [class_ $ base [Tokens.fgPrimary]]
            "Total"
          Lucid.span_
            [ class_ $ base [Tokens.fgPrimary],
              xText_ [i|selectedRatePrice === null ? '—' : formatCents(#{subtotalCents} + selectedRatePrice + #{taxCents})|]
            ]
            mempty
  where
    taxAmount = computeTax subtotal taxRate
    subtotalCents :: Int64
    subtotalCents = unCents subtotal
    taxCents :: Int64
    taxCents = unCents taxAmount

-- | Render a static total line (label + precomputed value).
renderTotalLine :: Text -> Text -> Lucid.Html ()
renderTotalLine label value =
  Lucid.div_
    [class_ $ base ["flex", "justify-between", Tokens.textSm, Tokens.py2]]
    $ do
      Lucid.span_ [class_ $ base [Tokens.fgMuted]] $ Lucid.toHtml label
      Lucid.span_ [class_ $ base [Tokens.fgPrimary]] $ Lucid.toHtml value

--------------------------------------------------------------------------------
-- Alpine.js state

-- | Alpine.js component state for the shipping rates fragment.
--
-- Tracks the selected rate ID and price (in cents) so the totals section
-- can update reactively. @proceedToPayment()@ must be wired up by the
-- parent checkout Alpine component via an event.
ratesAlpineState ::
  Cents ->
  Scientific ->
  Text
ratesAlpineState _subtotal _taxRate =
  [i|{
  selectedRateId: null,
  selectedRatePrice: null,
  formatCents(cents) {
    return '$' + (cents / 100).toFixed(2);
  },
  proceedToPayment() {
    if (this.selectedRateId === null) return;
    this.$dispatch('shipping-rate-selected', {
      rateId: this.selectedRateId,
      ratePrice: this.selectedRatePrice
    });
  }
}|]
