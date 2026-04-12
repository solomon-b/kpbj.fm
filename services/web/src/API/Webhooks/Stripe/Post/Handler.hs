{- HLINT ignore "Redundant id" -}

-- | Handler for POST /api/webhooks/stripe.
--
-- Receives Stripe webhook events, verifies the HMAC-SHA256 signature,
-- and dispatches on event type. Currently handles:
--
--   * @checkout.session.completed@ — marks the order as Paid, stores the
--     payment intent ID, and sends confirmation emails.
--
-- All business-logic failures (order not found, wrong status, etc.) are
-- logged and swallowed: we always return 200 to Stripe so it does not
-- retry events that succeeded at the HTTP layer.
--
-- Signature verification failures and parse errors still return 4xx so
-- Stripe knows to retry or alert.
module API.Webhooks.Stripe.Post.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardStoreOrdersLinks, rootLink)
import API.Types (DashboardStoreOrdersRoutes (..))
import App.BaseUrl qualified
import App.Monad (AppM)
import Control.Monad (when)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (eitherDecodeStrict')
import Data.ByteString (ByteString)
import Data.Has (getter)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Effects.Database.Execute (execQueryThrow)
import Effects.Database.Tables.OrderItems qualified as OrderItems
import Effects.Database.Tables.Orders qualified as Orders
import Effects.Database.Tables.StoreSettings qualified as StoreSettings
import Effects.Email.Send (sendAsync)
import Log qualified
import Servant qualified
import Store.Checkout.Emails
  ( OrderEmailData (..),
    OrderEmailItem (..),
    orderConfirmationEmail,
    staffNotificationEmail,
  )
import Stripe.Types
  ( CheckoutSession (..),
    StripeWebhookSecret,
    WebhookEvent (..),
    eventCheckoutSession,
  )
import Stripe.Webhook (verifyWebhookSignature)
import Utils (fromMaybeM)

--------------------------------------------------------------------------------

-- | Receive and process a Stripe webhook event.
--
-- Steps:
--
--   1. Require the @Stripe-Signature@ header — 400 if absent.
--   2. Require the webhook secret in the app context — 500 if unconfigured.
--   3. Verify the HMAC signature — 400 if invalid or replayed.
--   4. Parse the event body — 400 if malformed JSON.
--   5. Dispatch on event type; unknown types are logged and ignored.
--
-- Always returns 200 on success so Stripe does not retry successfully
-- processed events.
handler ::
  Maybe Text ->
  ByteString ->
  AppM Servant.NoContent
handler mSignature rawBody = do
  signature <-
    fromMaybeM
      (Log.logAttention "Stripe webhook received without Stripe-Signature header" () >> throwM Servant.err400)
      (pure mSignature)

  secret <-
    fromMaybeM
      (Log.logAttention "Stripe webhook secret not configured — cannot verify signature" () >> throwM Servant.err500)
      (asks (getter @(Maybe StripeWebhookSecret)))

  verifyResult <- liftIO $ verifyWebhookSignature secret signature rawBody
  case verifyResult of
    Left err -> do
      Log.logAttention "Stripe webhook signature verification failed" (show err)
      throwM Servant.err400
    Right () -> pure ()

  case eitherDecodeStrict' rawBody :: Either String WebhookEvent of
    Left parseErr -> do
      Log.logAttention "Failed to parse Stripe webhook event body" parseErr
      throwM Servant.err400
    Right evt -> handleEvent evt

  pure Servant.NoContent

--------------------------------------------------------------------------------
-- Event dispatch

-- | Dispatch on the Stripe event type.
--
-- Unknown event types are logged at info level and ignored — Stripe sends
-- many event types we do not subscribe to.
handleEvent :: WebhookEvent -> AppM ()
handleEvent evt
  | evt.type_ == "checkout.session.completed" =
      case eventCheckoutSession evt.data_ of
        Nothing ->
          Log.logAttention
            "checkout.session.completed: could not parse CheckoutSession from event data"
            ([("event_id", evt.id)] :: [(Text, Text)])
        Just session -> handleCheckoutCompleted session
  | otherwise =
      Log.logInfo "Stripe webhook event ignored" ([("type", evt.type_), ("id", evt.id)] :: [(Text, Text)])

--------------------------------------------------------------------------------
-- checkout.session.completed

-- | Handle a completed Stripe Checkout Session.
--
-- Looks up the order by checkout session ID. If the order is still in
-- 'Pending' status (idempotency guard), transitions it to 'Paid', stores
-- the payment intent ID, and fires confirmation emails.
--
-- All failures are logged but do not propagate — we have already verified
-- the signature and want Stripe to receive 200 regardless.
handleCheckoutCompleted :: CheckoutSession -> AppM ()
handleCheckoutCompleted session = do
  let sessionId = session.id
  mOrder <- execQueryThrow (Orders.getByStripeCheckoutSessionId sessionId)
  case mOrder of
    Nothing ->
      Log.logAttention
        "checkout.session.completed: no order found for Stripe checkout session"
        ([("session_id", sessionId)] :: [(Text, Text)])
    Just order ->
      when (order.oStatus == Orders.Pending) $ do
        _ <- execQueryThrow (Orders.updateStatus order.oId Orders.Paid)
        case session.paymentIntent of
          Just piId -> execQueryThrow (Orders.updateStripePaymentIntentId order.oId piId)
          Nothing -> pure ()
        Log.logInfo "Order marked as Paid via Stripe webhook" ([("order_number", order.oOrderNumber)] :: [(Text, Text)])
        sendOrderEmails order

--------------------------------------------------------------------------------
-- Email dispatch

-- | Build email data and fire order confirmation and staff notification emails.
--
-- Uses 'sendAsync' (fire-and-forget). Email failures are logged by the
-- email layer; they never block the webhook response.
sendOrderEmails :: Orders.Model -> AppM ()
sendOrderEmails order = do
  items <- execQueryThrow (OrderItems.getByOrderId order.oId)
  mSettings <- execQueryThrow StoreSettings.getSettings
  appBase <- App.BaseUrl.baseUrl
  let notificationEmail =
        maybe "contact@kpbj.fm" (.ssOrderNotificationEmail) mSettings
      orderDetailUrl = appBase <> rootLink (dashboardStoreOrdersLinks.detail order.oId)
      emailData = buildOrderEmailData orderDetailUrl order items
  sendAsync (orderConfirmationEmail emailData)
  sendAsync (staffNotificationEmail notificationEmail emailData)

--------------------------------------------------------------------------------
-- Data builders

-- | Assemble 'OrderEmailData' from the order row and its items.
buildOrderEmailData :: Text -> Orders.Model -> [OrderItems.Model] -> OrderEmailData
buildOrderEmailData orderDetailUrl order items =
  OrderEmailData
    { oedOrderNumber = order.oOrderNumber,
      oedEmail = order.oEmail,
      oedItems = map buildEmailItem items,
      oedSubtotalCents = order.oSubtotalCents,
      oedShippingCents = order.oShippingCents,
      oedShippingMethod = order.oShippingMethod,
      oedTaxCents = order.oTaxCents,
      oedTotalCents = order.oTotalCents,
      oedShippingFirstName = order.oShippingFirstName,
      oedShippingLastName = order.oShippingLastName,
      oedShippingAddressLine1 = order.oShippingAddressLine1,
      oedShippingAddressLine2 = order.oShippingAddressLine2,
      oedShippingCity = order.oShippingCity,
      oedShippingState = order.oShippingState,
      oedShippingZip = order.oShippingZip,
      oedOrderDetailUrl = orderDetailUrl,
      oedDate = formatOrderDate order.oCreatedAt
    }

-- | Map a single order-item row to the email item type.
buildEmailItem :: OrderItems.Model -> OrderEmailItem
buildEmailItem item =
  OrderEmailItem
    { oeiProductName = item.oiProductName,
      oeiVariantLabel = item.oiVariantLabel,
      oeiQuantity = item.oiQuantity,
      oeiUnitPriceCents = item.oiUnitPriceCents
    }

-- | Format a 'UTCTime' as a human-readable date string.
--
-- Example output: @"April 11, 2026"@
formatOrderDate :: UTCTime -> Text
formatOrderDate = Text.pack . formatTime defaultTimeLocale "%B %-d, %Y"
