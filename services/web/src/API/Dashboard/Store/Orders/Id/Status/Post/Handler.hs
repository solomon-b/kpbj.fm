{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Handler for @POST /dashboard/store/orders/:id/status@.
--
-- Updates the status of an order. Staff and above only.
--
-- When cancelling, inventory is restored for all line items.
-- When shipping, a tracking number is required and a shipping
-- confirmation email is sent fire-and-forget.
module API.Dashboard.Store.Orders.Id.Status.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Store.Orders.Id.Get.Templates (orderDetailContent)
import API.Dashboard.Store.Orders.Id.Status.Post.Route (StatusForm (..))
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleBannerErrors, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Execute (execQueryThrow)
import Effects.Database.Tables.OrderItems qualified as OrderItems
import Effects.Database.Tables.Orders qualified as Orders
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Effects.Database.Tables.Products qualified as Products
import Effects.Email.Send (sendAsync)
import Lucid qualified
import Store.Checkout.Emails (ShippingEmailData (..), shippingConfirmationEmail)
import Utils (fromMaybeM)

--------------------------------------------------------------------------------

-- | Servant handler: authenticate, validate, apply status transition.
handler ::
  Orders.Id ->
  Maybe Cookie ->
  StatusForm ->
  AppM (Lucid.Html ())
handler orderId cookie form =
  handleBannerErrors "Order status update" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to update order status." userMetadata

    -- Fetch the order; 404 if not found
    order <- fromMaybeM (throwNotFound "Order") $ lift (execQueryThrow (Orders.getById orderId))

    -- Parse and validate the target status
    targetStatus <- parseStatus form.sfStatus
    validateTransition order.oStatus targetStatus

    -- Side effects that depend on the target status
    case targetStatus of
      Orders.Cancelled -> do
        -- Restore inventory for each line item
        items <- lift $ execQueryThrow (OrderItems.getByOrderId orderId)
        lift $ for_ items $ \item ->
          case item.oiVariantId of
            Just vid -> execQueryThrow (ProductVariants.restoreInventory vid item.oiQuantity)
            Nothing -> execQueryThrow (Products.restoreInventory item.oiProductId item.oiQuantity)
      Orders.Shipped -> do
        -- Send shipping confirmation email with tracking number from the order
        -- (populated by EasyPost label purchase). If no tracking number exists,
        -- the email is still sent with an empty tracking field.
        let trackingNumber = order.oTrackingNumber
        lift $ sendShippingEmail order (fromMaybe "" trackingNumber)
      _ -> pure ()

    -- Persist the status change and re-fetch
    mUpdatedOrder <- lift $ execQueryThrow (Orders.updateStatus orderId targetStatus)
    updatedOrder <- fromMaybeM (throwNotFound "Order") $ pure mUpdatedOrder
    items <- lift $ execQueryThrow (OrderItems.getByOrderId orderId)

    let statusLabel = display targetStatus
    pure $ do
      orderDetailContent updatedOrder items
      renderBanner Success "Status Updated" [i|Order status changed to #{statusLabel}.|]

-- | Parse a raw status string into an OrderStatus, rejecting unknown values.
parseStatus :: Text -> ExceptT HandlerError AppM Orders.OrderStatus
parseStatus = \case
  "pending" -> pure Orders.Pending
  "paid" -> pure Orders.Paid
  "shipped" -> pure Orders.Shipped
  "completed" -> pure Orders.Completed
  "cancelled" -> pure Orders.Cancelled
  other -> throwValidationError [i|Invalid status: "#{other}".|]

-- | Validate that a status transition is legal.
--
-- Allowed transitions:
--   Pending  → Paid, Cancelled
--   Paid     → Shipped, Cancelled
--   Shipped  → Completed, Cancelled
--
-- All other transitions (including backwards and no-ops) are rejected.
validateTransition :: Orders.OrderStatus -> Orders.OrderStatus -> ExceptT HandlerError AppM ()
validateTransition from to =
  case (from, to) of
    (Orders.Pending, Orders.Paid) -> pure ()
    (Orders.Pending, Orders.Cancelled) -> pure ()
    (Orders.Paid, Orders.Shipped) -> pure ()
    (Orders.Paid, Orders.Cancelled) -> pure ()
    (Orders.Shipped, Orders.Completed) -> pure ()
    (Orders.Shipped, Orders.Cancelled) -> pure ()
    _ ->
      let fromLabel = display from
          toLabel = display to
       in throwValidationError [i|Cannot transition order from #{fromLabel} to #{toLabel}.|]

-- | Build and fire-and-forget a shipping confirmation email.
sendShippingEmail :: Orders.Model -> Text -> AppM ()
sendShippingEmail order trackingNumber =
  sendAsync $
    shippingConfirmationEmail
      ShippingEmailData
        { sedOrderNumber = order.oOrderNumber,
          sedEmail = order.oEmail,
          sedTrackingNumber = trackingNumber,
          -- Carrier is not known for manually-entered tracking numbers.
          sedCarrier = ""
        }
