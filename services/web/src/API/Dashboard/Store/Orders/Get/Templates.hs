{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Store.Orders.Get.Templates
  ( template,
    renderStatusBadge,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardStoreOrdersLinks, rootLink)
import API.Types (DashboardStoreOrdersRoutes (..))
import Component.Badge qualified as Badge
import Component.Card qualified as Card
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
  Card.section Card.TablePanel [Tokens.mb8] $
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
            Lucid.toHtml (display order.oEmail)

        Lucid.td_ (clickableCellAttrs detailUrl) $
          renderStatusBadge order.oStatus

        Lucid.td_ (clickableCellAttrs detailUrl) $
          Lucid.div_ [Lucid.class_ Tokens.textSm] $
            Lucid.toHtml (Cents.formatDisplay order.oTotalCents)

-- | The status badge of an order. The order list and the order detail page
-- both use it, so a status has one tone everywhere.
renderStatusBadge :: Orders.OrderStatus -> Lucid.Html ()
renderStatusBadge status =
  Badge.badge Badge.Regular (statusTone status) $ Lucid.toHtml (display status :: Text)

-- | Only a finished order is a success. A shipped order still waits for
-- delivery, so it is info. A refund is not a failure of the order, as a
-- cancel is, so it is a warning.
statusTone :: Orders.OrderStatus -> Badge.Tone
statusTone = \case
  Orders.Pending -> Badge.Warning
  Orders.Paid -> Badge.Info
  Orders.Shipped -> Badge.Info
  Orders.Completed -> Badge.Success
  Orders.Cancelled -> Badge.Error
  Orders.Refunded -> Badge.Warning

renderEmptyState :: Lucid.Html ()
renderEmptyState = do
  Card.card Card.EmptyState [] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Theme.fgMuted]] "No orders yet."
    Lucid.p_ [class_ $ base [Theme.fgMuted, "mt-2"]] "Orders will appear here once customers complete purchases."

formatDate :: UTCTime -> Text
formatDate t = [i|#{formatTime defaultTimeLocale "%b %-d, %Y" t}|]
