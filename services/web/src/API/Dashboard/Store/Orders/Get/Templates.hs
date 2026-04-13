{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Store.Orders.Get.Templates (template) where

--------------------------------------------------------------------------------

import API.Links (dashboardStoreOrdersLinks, rootLink)
import API.Types (DashboardStoreOrdersRoutes (..))
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    clickableCellAttrs,
    renderIndexTable,
    rowAttrs,
  )
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Domain.Types.Cents qualified as Cents
import Effects.Database.Tables.Orders qualified as Orders
import Lucid qualified

--------------------------------------------------------------------------------

-- | Orders list template.
template :: [Orders.Model] -> Lucid.Html ()
template orders =
  Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]] $
    if null orders
      then renderEmptyState
      else
        renderIndexTable
          IndexTableConfig
            { itcBodyId = "orders-table-body",
              itcHeaders =
                [ ColumnHeader "Order" AlignLeft,
                  ColumnHeader "Date" AlignLeft,
                  ColumnHeader "Customer" AlignLeft,
                  ColumnHeader "Status" AlignLeft,
                  ColumnHeader "Total" AlignRight
                ],
              itcNextPageUrl = Nothing,
              itcPaginationConfig = Nothing
            }
          (mapM_ renderOrderRow orders)

renderOrderRow :: Orders.Model -> Lucid.Html ()
renderOrderRow order =
  let orderId = order.oId
      orderIdText = display orderId
      rowId = [i|order-row-#{orderIdText}|] :: Text
      detailUrl = rootLink $ dashboardStoreOrdersLinks.detail orderId
   in Lucid.tr_ (rowAttrs rowId) $ do
        Lucid.td_ (clickableCellAttrs detailUrl) $
          Lucid.span_ [Lucid.class_ Tokens.fontBold] $
            Lucid.toHtml order.oOrderNumber

        Lucid.td_ (clickableCellAttrs detailUrl) $
          Lucid.div_ [Lucid.class_ Tokens.textSm] $
            Lucid.toHtml (formatDate order.oCreatedAt)

        Lucid.td_ (clickableCellAttrs detailUrl) $
          Lucid.div_ [Lucid.class_ Tokens.textSm] $
            Lucid.toHtml order.oEmail

        Lucid.td_ (clickableCellAttrs detailUrl) $
          renderStatusBadge order.oStatus

        Lucid.td_ (clickableCellAttrs detailUrl) $
          Lucid.div_ [Lucid.class_ Tokens.textSm] $
            Lucid.toHtml (Cents.formatDisplay order.oTotalCents)

renderStatusBadge :: Orders.OrderStatus -> Lucid.Html ()
renderStatusBadge status =
  let (bgClass, textClass) = statusColors status
      statusLabel = display status :: Text
   in Lucid.span_
        [class_ $ base ["inline-block", "px-3", "py-1", Tokens.textSm, Tokens.fontBold, "rounded", bgClass, textClass]]
        (Lucid.toHtml statusLabel)

statusColors :: Orders.OrderStatus -> (Text, Text)
statusColors = \case
  Orders.Pending -> (Tokens.warningBg, Tokens.warningText)
  Orders.Paid -> (Tokens.infoBg, Tokens.infoText)
  Orders.Shipped -> (Tokens.successBg, Tokens.successText)
  Orders.Completed -> (Tokens.successBg, Tokens.successText)
  Orders.Cancelled -> (Tokens.errorBg, Tokens.errorText)
  Orders.Refunded -> (Tokens.warningBg, Tokens.warningText)

renderEmptyState :: Lucid.Html ()
renderEmptyState = do
  Lucid.div_ [class_ $ base [Theme.bgAlt, Tokens.border2, Theme.borderMuted, "p-12", "text-center"]] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Theme.fgMuted]] "No orders yet."
    Lucid.p_ [class_ $ base [Theme.fgMuted, "mt-2"]] "Orders will appear here once customers complete purchases."

formatDate :: UTCTime -> Text
formatDate t = [i|#{formatTime defaultTimeLocale "%b %-d, %Y" t}|]
