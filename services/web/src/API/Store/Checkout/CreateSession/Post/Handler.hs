{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Handler for POST /api/store/checkout/create-session.
--
-- Receives JSON with checkout form data, validates the cart against the
-- database, verifies the selected shipping rate with EasyPost, creates a
-- pending order with inventory holds in a single transaction, creates a
-- Stripe Checkout Session, and returns the client secret + order number.
--
-- This is the most critical handler in the checkout flow. Every failure
-- mode must be handled explicitly:
--
-- * Cart items may have been deactivated or gone out of stock between
--   the checkout page load and form submission.
-- * The EasyPost shipment/rate may have expired.
-- * Inventory decrements must be atomic with order creation to prevent
--   overselling.
-- * If the Stripe call fails after the order is created, the order
--   remains in 'Pending' status and will be cleaned up by the stale
--   order reaper.
module API.Store.Checkout.CreateSession.Post.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import API.Links (rootLink, storeLinks)
import API.Store.Types (CartItem (..), CreateSessionRequest (..), CreateSessionResponse (..))
import API.Types (StoreRoutes (..))
import App.BaseUrl qualified
import App.Monad (AppM)
import Control.Monad (forM_, unless, when)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Aeson qualified as Aeson
import Data.Has (getter)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cents (Cents (..), unCents)
import EasyPost.Client (EasyPostClientError (..), getShipment)
import EasyPost.Types (EasyPostApiKey, Rate (..), Shipment (..))
import Effects.Database.Execute (execQueryThrow, execTransaction)
import Effects.Database.Tables.OrderItems qualified as OrderItems
import Effects.Database.Tables.Orders qualified as Orders
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.StoreSettings qualified as StoreSettings
import Hasql.Transaction qualified as HT
import Log qualified
import Network.HTTP.Client (Manager)
import Servant.Server qualified as Servant
import Store.Checkout.Logic (computeSubtotal, computeTax, computeTotal, easypostRateToCents)
import Stripe.Client (StripeClientError (..), createCheckoutSession)
import Stripe.Types
  ( CheckoutSession (..),
    CheckoutSessionCreate (..),
    Currency (..),
    FixedAmount (..),
    LineItem (..),
    PriceData (..),
    ProductData (..),
    ShippingOption (..),
    ShippingRateData (..),
    StripeSecretKey,
  )

--------------------------------------------------------------------------------

-- | A cart item fully resolved against the database.
--
-- All prices and names come from the DB, never from the client.
data ResolvedItem = ResolvedItem
  { riProductId :: !Products.Id,
    riVariantId :: !(Maybe ProductVariants.Id),
    riProductName :: !Text,
    riVariantLabel :: !Text,
    riQuantity :: !Int,
    riUnitPrice :: !Cents
  }

--------------------------------------------------------------------------------

-- | Handle a create-session request.
--
-- On success, returns JSON with the Stripe client secret and order number.
-- On failure, throws a Servant error with an appropriate status code.
handler ::
  CreateSessionRequest ->
  AppM CreateSessionResponse
handler req = do
  -- 1. Load store settings (need tax rate)
  mSettings <- execQueryThrow StoreSettings.getSettings
  settings <- maybe (throwServerError "Store settings are not configured.") pure mSettings

  -- 2. Validate and resolve all cart items against the database
  let cart = take 50 req.csrCart
  when (null cart) $
    throwBadRequest "Cart is empty."

  resolvedItems <- resolveCartItems cart

  -- 3. Verify the selected shipping rate with EasyPost
  manager <- asks (getter @Manager)
  easypostKey <- asks (getter @(Maybe EasyPostApiKey)) >>= maybe (throwServerError "Shipping is not configured.") pure

  shipmentResult <- liftIO $ getShipment manager easypostKey req.csrShippingShipmentId
  shipment <- case shipmentResult of
    Left (EasyPostClientError err) -> do
      Log.logAttention "EasyPost getShipment failed during checkout" (show err)
      throwServerError "Could not verify shipping rate. Please go back and try again."
    Right s -> pure s

  let mMatchedRate = find (\r -> r.id == req.csrShippingRateId) shipment.rates
  matchedRate <- maybe (throwBadRequest "Selected shipping rate is no longer available.") pure mMatchedRate

  shippingCents <- case easypostRateToCents matchedRate.rate of
    Nothing -> do
      Log.logAttention "Failed to parse EasyPost rate to cents" matchedRate.rate
      throwServerError "Could not parse shipping rate."
    Just c -> pure c

  let shippingDisplayName = matchedRate.carrier <> " " <> matchedRate.service

  -- 4. Compute totals
  let priceQtyPairs =
        [ (ri.riUnitPrice, fromIntegral ri.riQuantity)
          | ri <- resolvedItems
        ]
      subtotal = computeSubtotal priceQtyPairs
      tax = computeTax subtotal settings.ssTaxRate
      total = computeTotal subtotal shippingCents tax

  -- 5. Generate order number
  mOrderNumber <- execQueryThrow Orders.nextOrderNumber
  orderNumber <- maybe (throwServerError "Failed to generate order number.") pure mOrderNumber

  -- 6. DB transaction: insert order + items + decrement inventory
  --
  -- If any inventory decrement fails, the transaction aborts and no
  -- order is created. This prevents overselling.
  let orderInsert =
        Orders.Insert
          { oiOrderNumber = orderNumber,
            oiEmail = req.csrEmail,
            oiShippingFirstName = req.csrFirstName,
            oiShippingLastName = req.csrLastName,
            oiShippingAddressLine1 = req.csrAddressLine1,
            oiShippingAddressLine2 = req.csrAddressLine2,
            oiShippingCity = req.csrCity,
            oiShippingState = req.csrState,
            oiShippingZip = req.csrZip,
            oiShippingCountry = "US",
            oiShippingMethod = shippingDisplayName,
            oiSubtotalCents = subtotal,
            oiShippingCents = shippingCents,
            oiTaxCents = tax,
            oiTotalCents = total,
            oiPaymentMethod = Orders.Stripe,
            oiStripeCheckoutSessionId = Nothing
          }

  txResult <- execTransaction $ runExceptT $ do
    orderId <-
      ExceptT $
        maybe (Left "Failed to create order.") Right
          <$> HT.statement () (Orders.insertOrder orderInsert)

    forM_ resolvedItems $ \item -> do
      let itemInsert =
            OrderItems.Insert
              { iiOrderId = orderId,
                iiProductId = item.riProductId,
                iiVariantId = item.riVariantId,
                iiProductName = item.riProductName,
                iiVariantLabel = item.riVariantLabel,
                iiQuantity = fromIntegral item.riQuantity,
                iiUnitPriceCents = item.riUnitPrice
              }
      _ <- lift $ HT.statement () (OrderItems.insertOrderItem itemInsert)

      -- Decrement inventory. Returns Nothing if insufficient stock or inactive.
      decremented <- lift $ case item.riVariantId of
        Just vid -> isJust <$> HT.statement () (ProductVariants.decrementInventory vid (fromIntegral item.riQuantity))
        Nothing -> isJust <$> HT.statement () (Products.decrementInventory item.riProductId (fromIntegral item.riQuantity))
      unless decremented $ do
        lift HT.condemn
        throwE (item.riProductName <> " is out of stock.")

    pure orderId

  orderId <- case txResult of
    Left dbErr -> throwServerError ("Database error: " <> Text.pack (show dbErr))
    Right (Left msg) -> throwBadRequest msg
    Right (Right oid) -> pure oid

  -- 7. Create Stripe Checkout Session
  appBase <- App.BaseUrl.baseUrl
  stripeKey <- asks (getter @(Maybe StripeSecretKey)) >>= maybe (throwServerError "Payment processing is not configured.") pure

  let productLineItems =
        [ LineItem
            { priceData =
                PriceData
                  { currency = USD,
                    unitAmount = fromIntegral (unCents item.riUnitPrice),
                    productData =
                      ProductData
                        { name = mkLineItemName item,
                          description = Nothing
                        }
                  },
              quantity = item.riQuantity
            }
          | item <- resolvedItems
        ]

      taxLineItem =
        LineItem
          { priceData =
              PriceData
                { currency = USD,
                  unitAmount = fromIntegral (unCents tax),
                  productData =
                    ProductData
                      { name = "Sales Tax",
                        description = Nothing
                      }
                },
            quantity = 1
          }

      stripeReq =
        CheckoutSessionCreate
          { mode = "payment",
            uiMode = "embedded",
            returnUrl = appBase <> rootLink (storeLinks.orderConfirmation orderNumber) <> "?session_id={CHECKOUT_SESSION_ID}",
            customerEmail = Just req.csrEmail,
            lineItems = productLineItems <> [taxLineItem],
            shippingOptions =
              [ ShippingOption
                  { shippingRateData =
                      ShippingRateData
                        { type_ = "fixed_amount",
                          displayName = shippingDisplayName,
                          fixedAmount =
                            FixedAmount
                              { amount = fromIntegral (unCents shippingCents),
                                currency = USD
                              }
                        }
                  }
              ],
            paymentMethodTypes = ["card"],
            metadata = Map.fromList [("order_number", orderNumber)]
          }

  sessionResult <- liftIO $ createCheckoutSession manager stripeKey stripeReq
  session <- case sessionResult of
    Left (StripeClientError err) -> do
      Log.logAttention "Stripe createCheckoutSession failed" (show err)
      -- The order exists in Pending state. The stale order reaper will
      -- cancel it and restore inventory after 30 minutes.
      throwServerError "Payment session could not be created. Please try again."
    Right s -> pure s

  -- 8. Store Stripe checkout session ID on the order
  execQueryThrow (Orders.updateStripeCheckoutSessionId orderId session.id)

  Log.logInfo "Checkout session created" orderNumber

  -- 9. Return response
  pure
    CreateSessionResponse
      { csrespClientSecret = fromMaybe "" session.clientSecret,
        csrespOrderNumber = orderNumber
      }

--------------------------------------------------------------------------------
-- Cart Resolution

-- | Resolve all cart items against the database.
--
-- Throws a 400 error if any item is unavailable, inactive, or has a
-- variant that doesn't belong to the product.
resolveCartItems ::
  [CartItem] ->
  AppM [ResolvedItem]
resolveCartItems = traverse resolveOne

-- | Resolve a single cart item.
resolveOne ::
  CartItem ->
  AppM ResolvedItem
resolveOne cartItem = do
  mProduct <- execQueryThrow $ Products.getById cartItem.productId
  product' <- maybe (throwBadRequest "One or more items in your cart are no longer available.") pure mProduct

  let productName = product'.pName

  unless product'.pIsActive $
    throwBadRequest (productName <> " is no longer available.")

  case cartItem.variantId of
    Nothing ->
      pure
        ResolvedItem
          { riProductId = product'.pId,
            riVariantId = Nothing,
            riProductName = productName,
            riVariantLabel = "",
            riQuantity = cartItem.quantity,
            riUnitPrice = product'.pBasePriceCents
          }
    Just vid -> resolveVariant product' vid cartItem.quantity

-- | Resolve a variant for a cart item.
resolveVariant ::
  Products.Model ->
  ProductVariants.Id ->
  Int ->
  AppM ResolvedItem
resolveVariant product' variantId qty = do
  let productName = product'.pName

  mVariant <- execQueryThrow $ ProductVariants.getById variantId
  variant <- maybe (throwBadRequest ("A selected option for " <> productName <> " is no longer available.")) pure mVariant

  when (variant.pvProductId /= product'.pId) $
    throwBadRequest ("A selected option for " <> productName <> " is no longer available.")

  when (isJust variant.pvDeletedAt) $
    throwBadRequest ("The " <> variant.pvLabel <> " option for " <> productName <> " is no longer available.")

  pure
    ResolvedItem
      { riProductId = product'.pId,
        riVariantId = Just variantId,
        riProductName = productName,
        riVariantLabel = variant.pvLabel,
        riQuantity = qty,
        riUnitPrice = fromMaybe product'.pBasePriceCents variant.pvPriceCents
      }

--------------------------------------------------------------------------------
-- Helpers

-- | Build a display name for a Stripe line item.
--
-- If the item has a variant label, appends it in parentheses.
mkLineItemName :: ResolvedItem -> Text
mkLineItemName item
  | item.riVariantLabel == "" = item.riProductName
  | otherwise = item.riProductName <> " (" <> item.riVariantLabel <> ")"

-- | Throw a 400 Bad Request with a JSON error body.
throwBadRequest :: Text -> AppM a
throwBadRequest msg =
  throwM
    Servant.err400
      { Servant.errBody = Aeson.encode (Aeson.object ["error" Aeson..= msg])
      }

-- | Throw a 500 Internal Server Error with a JSON error body.
throwServerError :: Text -> AppM a
throwServerError msg = do
  Log.logAttention "Checkout server error" msg
  throwM
    Servant.err500
      { Servant.errBody = Aeson.encode (Aeson.object ["error" Aeson..= msg])
      }
