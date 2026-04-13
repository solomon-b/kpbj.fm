{-# LANGUAGE OverloadedRecordDot #-}

-- | Templates for @POST /dashboard/store/orders/:id/label@.
--
-- Two fragments are produced depending on which step of the label flow
-- completed:
--
--  * 'ratesTemplate' — returned from step 1; renders rate radio buttons
--    inside a form that posts back to the same endpoint.
--
--  * 'successTemplate' — returned from step 2; replaces @#label-section@
--    with the purchased tracking number and label download link.
module API.Dashboard.Store.Orders.Id.Label.Post.Templates
  ( ratesTemplate,
    successTemplate,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardStoreOrdersLinks, rootLink)
import API.Types (DashboardStoreOrdersRoutes (..))
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as Text
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cents qualified as Cents
import EasyPost.Types (Rate (..), Shipment (..))
import Effects.Database.Tables.Orders qualified as Orders
import Lucid qualified
import Lucid.Base qualified as LucidBase
import Lucid.HTMX (hxPost_, hxTarget_)
import Store.Checkout.Logic (easypostRateToCents)

--------------------------------------------------------------------------------

-- | Rate selection fragment (step 1).
--
-- Renders inside @#label-section@. The form posts back to the label endpoint
-- with the chosen @rate_id@ and the shipment's @shipment_id@.
ratesTemplate ::
  -- | The EasyPost shipment returned from createShipment
  Shipment ->
  -- | Rates sorted cheapest-first
  [Rate] ->
  -- | Order ID (used to build the form action URL)
  Orders.Id ->
  Lucid.Html ()
ratesTemplate shipment rates orderId =
  Lucid.div_ [class_ $ base [Tokens.mb4]] $ do
    Lucid.h3_
      [class_ $ base [Tokens.fontBold, Tokens.mb4, "uppercase", Tokens.textSm, Tokens.fgMuted]]
      "Select Shipping Rate"
    case rates of
      [] ->
        Lucid.p_
          [class_ $ base [Tokens.textSm, Tokens.fgMuted]]
          "No rates available for this address."
      _ ->
        Lucid.form_
          [ hxPost_ labelUrl,
            hxTarget_ "#label-section"
          ]
          $ do
            Lucid.input_
              [ Lucid.type_ "hidden",
                Lucid.name_ "shipment_id",
                Lucid.value_ shipment.id
              ]
            Lucid.div_
              [class_ $ base [Tokens.mb4]]
              $ for_ rates renderRateOption
            Lucid.button_
              [ Lucid.type_ "submit",
                class_ $
                  base
                    [ Tokens.bgAlt,
                      Tokens.fgPrimary,
                      Tokens.border2,
                      Tokens.borderDefault,
                      Tokens.px4,
                      Tokens.py2,
                      Tokens.textSm,
                      Tokens.fontBold,
                      Tokens.hoverBg
                    ]
              ]
              "Purchase Label"
  where
    labelUrl :: Text
    labelUrl = rootLink $ dashboardStoreOrdersLinks.purchaseLabel orderId

-- | Render a single rate radio button row.
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
              Lucid.name_ "rate_id",
              Lucid.value_ r.id,
              Lucid.required_ "true",
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
    displayPrice :: Text
    displayPrice =
      maybe ("$" <> r.rate) Cents.formatDisplay (easypostRateToCents r.rate)

    deliveryDaysLabel :: Int -> Text
    deliveryDaysLabel days =
      "(" <> Text.pack (show days) <> " day" <> (if days == 1 then "" else "s") <> ")"

--------------------------------------------------------------------------------

-- | Success fragment (step 2).
--
-- Replaces the contents of @#label-section@ with the purchased tracking
-- number and an optional label download link. Also emits an OOB success
-- banner.
successTemplate ::
  -- | Tracking number (may be empty if EasyPost did not return one)
  Text ->
  -- | Label download URL, if available
  Maybe Text ->
  Lucid.Html ()
successTemplate trackingNumber mLabelUrl = do
  renderBanner'
  Lucid.div_
    [ class_ $ base [Tokens.bgAlt, Tokens.p4, "border", Tokens.borderDefault, "space-y-2"]
    ]
    $ do
      Lucid.p_
        [class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.successText]]
        "Label purchased successfully."
      if Text.null trackingNumber
        then pure ()
        else Lucid.div_ $ do
          Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] "Tracking Number: "
          Lucid.span_ [class_ $ base [Tokens.textSm, "font-mono"]] $
            Lucid.toHtml trackingNumber
      case mLabelUrl of
        Nothing -> pure ()
        Just labelUrl ->
          Lucid.div_ $
            Lucid.a_
              [ Lucid.href_ labelUrl,
                Lucid.download_ "",
                class_ $
                  base
                    [ Tokens.textSm,
                      Tokens.fontBold,
                      Tokens.successText,
                      "underline"
                    ]
              ]
              "Download Shipping Label"
  where
    -- OOB success banner
    renderBanner' :: Lucid.Html ()
    renderBanner' =
      Lucid.div_
        [ Lucid.id_ "banner-container",
          LucidBase.makeAttributes "hx-swap-oob" "true",
          Lucid.class_ Tokens.fullWidth
        ]
        $ Lucid.div_
          [ Lucid.id_ "banner",
            class_ $
              base
                [ Tokens.successBg,
                  Tokens.border2,
                  Tokens.successBorder,
                  Tokens.p4,
                  Tokens.mb6,
                  Tokens.fullWidth
                ]
          ]
        $ Lucid.div_
          [class_ $ base ["flex", "items-center", "justify-between"]]
        $ Lucid.div_
          [class_ $ base ["flex", "items-center", Tokens.gap4]]
        $ do
          Lucid.span_ [Lucid.class_ Tokens.text2xl] "✓"
          Lucid.div_ $ do
            Lucid.h3_ [class_ $ base [Tokens.fontBold, Tokens.successText]] "Label Purchased"
            Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.successText]] "Shipping label purchased and saved."
