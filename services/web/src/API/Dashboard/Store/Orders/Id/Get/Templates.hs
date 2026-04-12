{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Templates for @GET /dashboard/store/orders/:id@.
--
-- Renders a full order detail view with status, line items, shipping address,
-- order totals, Stripe info, tracking, notes, and conditional action buttons.
module API.Dashboard.Store.Orders.Id.Get.Templates
  ( template,
    orderDetailContent,
    renderNotesSection,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardStoreOrdersLinks, rootLink)
import API.Types (DashboardStoreOrdersRoutes (..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cents (Cents (..))
import Domain.Types.Cents qualified as Cents
import Effects.Database.Tables.OrderItems qualified as OrderItems
import Effects.Database.Tables.Orders qualified as Orders
import Lucid qualified
import Lucid.HTMX (hxPost_, hxSwap_, hxTarget_)

--------------------------------------------------------------------------------

-- | Full order detail template with the @#order-detail@ wrapper div.
--
-- Used by the GET handler to render the initial page.
template :: Orders.Model -> [OrderItems.Model] -> Lucid.Html ()
template order items =
  Lucid.div_
    [ Lucid.id_ "order-detail",
      class_ $ base [Tokens.bgMain, Tokens.p6]
    ]
    (orderDetailContent order items)

-- | Inner order detail content without the wrapper div.
--
-- Used by the status POST handler to re-render the detail after a
-- status transition (swapped into @#order-detail@ via HTMX innerHTML).
orderDetailContent :: Orders.Model -> [OrderItems.Model] -> Lucid.Html ()
orderDetailContent order items = do
  renderOrderHeader order
  renderItemsTable items
  renderShippingAddress order
  renderOrderTotals order
  renderStripeInfo order
  renderTrackingSection order
  renderNotesSection order
  renderActionButtons order

renderOrderHeader :: Orders.Model -> Lucid.Html ()
renderOrderHeader order =
  Lucid.div_ [class_ $ base [Tokens.mb8]] $ do
    Lucid.div_
      [class_ $ base ["flex", "items-start", "justify-between", Tokens.mb4]]
      $ do
        Lucid.div_ $ do
          Lucid.h1_
            [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb4]]
            $ Lucid.toHtml order.oOrderNumber
          Lucid.div_
            [class_ $ base ["flex", "items-center", Tokens.gap4]]
            $ do
              renderStatusBadge order.oStatus
              Lucid.span_
                [class_ $ base [Tokens.textSm, Tokens.fgMuted]]
                $ Lucid.toHtml (formatOrderDate order.oCreatedAt)
        Lucid.div_
          [class_ $ base [Tokens.textSm, Tokens.fgMuted]]
          $ Lucid.toHtml order.oEmail

renderStatusBadge :: Orders.OrderStatus -> Lucid.Html ()
renderStatusBadge status =
  let (bgClass, textClass) = statusColors status
   in Lucid.span_
        [ class_ $
            base
              [ "inline-block",
                "px-3",
                "py-1",
                Tokens.textSm,
                Tokens.fontBold,
                "rounded",
                bgClass,
                textClass
              ]
        ]
        $ Lucid.toHtml (display status)

statusColors :: Orders.OrderStatus -> (Text, Text)
statusColors = \case
  Orders.Pending -> (Tokens.warningBg, Tokens.warningText)
  Orders.Paid -> (Tokens.infoBg, Tokens.infoText)
  Orders.Shipped -> (Tokens.infoBg, Tokens.infoText)
  Orders.Completed -> (Tokens.successBg, Tokens.successText)
  Orders.Cancelled -> (Tokens.errorBg, Tokens.errorText)
  Orders.Refunded -> (Tokens.errorBg, Tokens.errorText)

renderItemsTable :: [OrderItems.Model] -> Lucid.Html ()
renderItemsTable items =
  Lucid.div_ [class_ $ base [Tokens.mb8]] $ do
    Lucid.h2_
      [class_ $ base [Tokens.fontBold, Tokens.mb4, "uppercase", Tokens.textSm, Tokens.fgMuted]]
      "Items"
    Lucid.table_ [class_ $ base ["w-full", "border", Tokens.borderDefault]] $ do
      Lucid.thead_ $ do
        Lucid.tr_
          [class_ $ base [Tokens.bgAlt, Tokens.textSm, Tokens.fontBold, "uppercase"]]
          $ do
            Lucid.th_ (thAttrs AlignLeft) "Product"
            Lucid.th_ (thAttrs AlignLeft) "Variant"
            Lucid.th_ (thAttrs AlignRight) "Qty"
            Lucid.th_ (thAttrs AlignRight) "Unit Price"
            Lucid.th_ (thAttrs AlignRight) "Total"
      Lucid.tbody_ $ mapM_ renderItemRow items

renderItemRow :: OrderItems.Model -> Lucid.Html ()
renderItemRow item =
  let lineTotal = Cents (item.oiUnitPriceCents.unCents * item.oiQuantity)
   in Lucid.tr_
        [class_ $ base ["border-t", Tokens.borderMuted]]
        $ do
          Lucid.td_ (tdAttrs AlignLeft) $ Lucid.toHtml item.oiProductName
          Lucid.td_ (tdAttrs AlignLeft) $ Lucid.toHtml item.oiVariantLabel
          Lucid.td_ (tdAttrs AlignRight) $ Lucid.toHtml (display item.oiQuantity)
          Lucid.td_ (tdAttrs AlignRight) $ Lucid.toHtml (Cents.formatDisplay item.oiUnitPriceCents)
          Lucid.td_ (tdAttrs AlignRight) $ Lucid.toHtml (Cents.formatDisplay lineTotal)

data ColAlign = AlignLeft | AlignRight

thAttrs :: ColAlign -> [Lucid.Attributes]
thAttrs align =
  [ class_ $ base ([Tokens.p2, Tokens.textSm] <> alignClass align)
  ]

tdAttrs :: ColAlign -> [Lucid.Attributes]
tdAttrs align =
  [ class_ $ base ([Tokens.p2, Tokens.textSm] <> alignClass align)
  ]

alignClass :: ColAlign -> [Text]
alignClass AlignLeft = ["text-left"]
alignClass AlignRight = ["text-right"]

renderShippingAddress :: Orders.Model -> Lucid.Html ()
renderShippingAddress order =
  Lucid.div_ [class_ $ base [Tokens.mb8]] $ do
    Lucid.h2_
      [class_ $ base [Tokens.fontBold, Tokens.mb4, "uppercase", Tokens.textSm, Tokens.fgMuted]]
      "Shipping Address"
    Lucid.div_ [class_ $ base [Tokens.bgAlt, Tokens.p4, "border", Tokens.borderDefault]] $ do
      Lucid.p_ [class_ $ base [Tokens.textSm]] $
        Lucid.toHtml (order.oShippingFirstName <> " " <> order.oShippingLastName)
      Lucid.p_ [class_ $ base [Tokens.textSm]] $
        Lucid.toHtml order.oShippingAddressLine1
      let line2 = order.oShippingAddressLine2
      if Text.null line2
        then pure ()
        else Lucid.p_ [class_ $ base [Tokens.textSm]] $ Lucid.toHtml line2
      Lucid.p_ [class_ $ base [Tokens.textSm]] $
        Lucid.toHtml
          ( order.oShippingCity
              <> ", "
              <> order.oShippingState
              <> " "
              <> order.oShippingZip
          )
      Lucid.p_ [class_ $ base [Tokens.textSm]] $
        Lucid.toHtml order.oShippingCountry

renderOrderTotals :: Orders.Model -> Lucid.Html ()
renderOrderTotals order =
  let shippingMethod = order.oShippingMethod
      shippingLabel = [i|Shipping (#{shippingMethod})|] :: Text
   in Lucid.div_ [class_ $ base [Tokens.mb8]] $ do
        Lucid.h2_
          [class_ $ base [Tokens.fontBold, Tokens.mb4, "uppercase", Tokens.textSm, Tokens.fgMuted]]
          "Order Totals"
        Lucid.div_
          [class_ $ base [Tokens.bgAlt, Tokens.p4, "border", Tokens.borderDefault, "space-y-1"]]
          $ do
            renderTotalLine "Subtotal" (Cents.formatDisplay order.oSubtotalCents)
            renderTotalLine shippingLabel (Cents.formatDisplay order.oShippingCents)
            renderTotalLine "Tax" (Cents.formatDisplay order.oTaxCents)
            Lucid.div_
              [class_ $ base ["flex", "justify-between", "border-t", Tokens.borderDefault, "pt-1", "mt-1"]]
              $ do
                Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fontBold]] "Total"
                Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fontBold]] $
                  Lucid.toHtml (Cents.formatDisplay order.oTotalCents)

renderTotalLine :: Text -> Text -> Lucid.Html ()
renderTotalLine label amount =
  Lucid.div_ [class_ $ base ["flex", "justify-between"]] $ do
    Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] $ Lucid.toHtml label
    Lucid.span_ [class_ $ base [Tokens.textSm]] $ Lucid.toHtml amount

renderStripeInfo :: Orders.Model -> Lucid.Html ()
renderStripeInfo order =
  case order.oStripePaymentIntentId of
    Nothing -> pure ()
    Just piId ->
      Lucid.div_ [class_ $ base [Tokens.mb8]] $ do
        Lucid.h2_
          [class_ $ base [Tokens.fontBold, Tokens.mb4, "uppercase", Tokens.textSm, Tokens.fgMuted]]
          "Stripe"
        Lucid.div_
          [class_ $ base [Tokens.bgAlt, Tokens.p4, "border", Tokens.borderDefault]]
          $ do
            Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] "Payment Intent ID"
            Lucid.p_ [class_ $ base [Tokens.textSm, "font-mono"]] $ Lucid.toHtml piId

renderTrackingSection :: Orders.Model -> Lucid.Html ()
renderTrackingSection order =
  Lucid.div_ [Lucid.id_ "label-section", class_ $ base [Tokens.mb8]] $ do
    Lucid.h2_
      [class_ $ base [Tokens.fontBold, Tokens.mb4, "uppercase", Tokens.textSm, Tokens.fgMuted]]
      "Tracking"
    case order.oTrackingNumber of
      Nothing ->
        Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] "No tracking information yet."
      Just trackingNum ->
        Lucid.div_
          [class_ $ base [Tokens.bgAlt, Tokens.p4, "border", Tokens.borderDefault, "space-y-2"]]
          $ do
            Lucid.div_ $ do
              Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] "Tracking Number: "
              Lucid.span_ [class_ $ base [Tokens.textSm, "font-mono"]] $ Lucid.toHtml trackingNum
            case order.oLabelUrl of
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

renderNotesSection :: Orders.Model -> Lucid.Html ()
renderNotesSection order =
  let notesUrl = rootLink $ dashboardStoreOrdersLinks.notes order.oId
   in Lucid.div_ [Lucid.id_ "notes-section", class_ $ base [Tokens.mb8]] $ do
        Lucid.h2_
          [class_ $ base [Tokens.fontBold, Tokens.mb4, "uppercase", Tokens.textSm, Tokens.fgMuted]]
          "Notes"
        Lucid.form_
          [ hxPost_ notesUrl,
            hxTarget_ "#notes-section",
            hxSwap_ "outerHTML",
            class_ $ base ["space-y-2"]
          ]
          $ do
            Lucid.textarea_
              [ Lucid.name_ "notes",
                Lucid.rows_ "4",
                class_ $
                  base
                    [ "w-full",
                      Tokens.bgMain,
                      Tokens.fgPrimary,
                      "border",
                      Tokens.borderDefault,
                      Tokens.p2,
                      Tokens.textSm,
                      "font-mono",
                      "resize-y"
                    ]
              ]
              $ Lucid.toHtml order.oNotes
            Lucid.button_
              [ Lucid.type_ "submit",
                class_ $
                  base
                    [ Tokens.bgAlt,
                      Tokens.fgPrimary,
                      "border",
                      Tokens.borderDefault,
                      Tokens.px4,
                      Tokens.py2,
                      Tokens.textSm,
                      Tokens.fontBold,
                      Tokens.hoverBg
                    ]
              ]
              "Save Notes"

renderActionButtons :: Orders.Model -> Lucid.Html ()
renderActionButtons order =
  case order.oStatus of
    Orders.Completed -> pure ()
    Orders.Cancelled -> pure ()
    Orders.Refunded -> pure ()
    _ ->
      Lucid.div_ [class_ $ base [Tokens.mb8]] $ do
        Lucid.h2_
          [class_ $ base [Tokens.fontBold, Tokens.mb4, "uppercase", Tokens.textSm, Tokens.fgMuted]]
          "Actions"
        Lucid.div_ [class_ $ base ["flex", "flex-wrap", Tokens.gap4]] $
          renderStatusActions order

renderStatusActions :: Orders.Model -> Lucid.Html ()
renderStatusActions order =
  let statusUrl = rootLink $ dashboardStoreOrdersLinks.status order.oId
      labelUrl = rootLink $ dashboardStoreOrdersLinks.purchaseLabel order.oId
   in case order.oStatus of
        Orders.Pending -> do
          statusActionForm statusUrl "paid" "Mark as Paid"
          cancelForm statusUrl
        Orders.Paid -> do
          buyLabelForm labelUrl
          statusActionForm statusUrl "shipped" "Mark as Shipped"
          cancelForm statusUrl
        Orders.Shipped -> do
          statusActionForm statusUrl "completed" "Mark as Completed"
          cancelForm statusUrl
        _ -> pure ()

-- | Simple status transition button (no extra inputs).
statusActionForm :: Text -> Text -> Text -> Lucid.Html ()
statusActionForm url statusValue buttonLabel =
  Lucid.form_
    [ hxPost_ url,
      hxSwap_ "innerHTML",
      hxTarget_ "#order-detail"
    ]
    $ do
      Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "status", Lucid.value_ statusValue]
      Lucid.button_
        [ Lucid.type_ "submit",
          class_ $
            base
              [ Tokens.bgAlt,
                Tokens.fgPrimary,
                "border",
                Tokens.borderDefault,
                Tokens.px4,
                Tokens.py2,
                Tokens.textSm,
                Tokens.fontBold,
                Tokens.hoverBg
              ]
        ]
        $ Lucid.toHtml buttonLabel

-- | Cancel order form.
cancelForm :: Text -> Lucid.Html ()
cancelForm url =
  Lucid.form_
    [ hxPost_ url,
      hxSwap_ "innerHTML",
      hxTarget_ "#order-detail"
    ]
    $ do
      Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "status", Lucid.value_ "cancelled"]
      Lucid.button_
        [ Lucid.type_ "submit",
          class_ $
            base
              [ Tokens.errorBg,
                Tokens.errorText,
                "border",
                Tokens.errorBorder,
                Tokens.px4,
                Tokens.py2,
                Tokens.textSm,
                Tokens.fontBold
              ]
        ]
        "Cancel Order"

-- | Buy shipping label form — posts to label endpoint.
buyLabelForm :: Text -> Lucid.Html ()
buyLabelForm url =
  Lucid.form_
    [ hxPost_ url,
      hxTarget_ "#label-section"
    ]
    $ do
      Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "rate_id", Lucid.value_ ""]
      Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "shipment_id", Lucid.value_ ""]
      Lucid.button_
        [ Lucid.type_ "submit",
          class_ $
            base
              [ Tokens.bgAlt,
                Tokens.fgPrimary,
                "border",
                Tokens.borderDefault,
                Tokens.px4,
                Tokens.py2,
                Tokens.textSm,
                Tokens.fontBold,
                Tokens.hoverBg
              ]
        ]
        "Buy Shipping Label"

--------------------------------------------------------------------------------
-- Helpers

-- | Format a UTC timestamp for display.
formatOrderDate :: UTCTime -> Text
formatOrderDate = Text.pack . formatTime defaultTimeLocale "%B %-d, %Y at %H:%M UTC"
