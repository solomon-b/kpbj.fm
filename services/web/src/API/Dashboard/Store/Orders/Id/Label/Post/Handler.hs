{-# LANGUAGE OverloadedRecordDot #-}

-- | Handler for @POST /dashboard/store/orders/:id/label@.
--
-- Two-step HTMX flow for purchasing EasyPost shipping labels.
--
-- Step 1 (empty rate_id): Creates an EasyPost shipment from the order's
-- shipping address and line item weights, then returns an HTML fragment
-- with rate radio buttons.
--
-- Step 2 (non-empty rate_id): Buys the selected rate, stores the tracking
-- number and label URL on the order, and returns a success fragment.
module API.Dashboard.Store.Orders.Id.Label.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Store.Orders.Id.Label.Post.Route (LabelForm (..))
import API.Dashboard.Store.Orders.Id.Label.Post.Templates qualified as Templates
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleBannerErrors, throwHandlerFailure, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has (getter)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import EasyPost.Client (EasyPostClientError (..), buyShipment, createShipment)
import EasyPost.Types
  ( Address (..),
    EasyPostApiKey,
    Label (..),
    Parcel (..),
    Shipment (..),
    ShipmentBuy (..),
    ShipmentCreate (..),
  )
import Effects.Database.Execute (execQueryThrow)
import Effects.Database.Tables.OrderItems qualified as OrderItems
import Effects.Database.Tables.Orders qualified as Orders
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.StoreSettings qualified as StoreSettings
import Log qualified
import Lucid qualified
import Network.HTTP.Client (Manager)
import Store.Checkout.Logic (sortRatesByPrice)
import Store.Checkout.ShippingErrors (easyPostFailureMessage)
import Utils (fromMaybeM)

--------------------------------------------------------------------------------

-- | Servant handler: authenticate, then dispatch to step 1 or step 2.
handler ::
  Orders.Id ->
  Maybe Cookie ->
  LabelForm ->
  AppM (Lucid.Html ())
handler orderId cookie form =
  handleBannerErrors "Label purchase" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to purchase shipping labels." userMetadata

    order <- fromMaybeM (throwNotFound "Order") $ lift (execQueryThrow (Orders.getById orderId))

    if Text.null (lfRateId form)
      then createShipmentAndShowRates order
      else buyLabelAndUpdate orderId form

-- | Step 1: compute total weight, create EasyPost shipment, return rate HTML.
createShipmentAndShowRates ::
  Orders.Model ->
  ExceptT HandlerError AppM (Lucid.Html ())
createShipmentAndShowRates order = do
  settings <-
    fromMaybeM (throwHandlerFailure "Store settings are not configured.") $
      lift (execQueryThrow StoreSettings.getSettings)

  apiKey <-
    fromMaybeM (throwHandlerFailure "EasyPost API key is not configured.") $
      lift (asks (getter @(Maybe EasyPostApiKey)))

  items <- lift $ execQueryThrow (OrderItems.getByOrderId order.oId)
  totalWeight <- computeTotalWeight items

  manager <- lift $ asks (getter @Manager)

  let shipmentCreate = buildShipmentCreate order settings totalWeight
  result <- liftIO $ createShipment manager apiKey shipmentCreate

  case result of
    Left (EasyPostClientError err) -> do
      lift $ Log.logInfo "EasyPost createShipment failed" (show err)
      pure $ renderBanner Error "Shipping Error" (easyPostFailureMessage (EasyPostClientError err))
    Right shipment ->
      pure $ Templates.ratesTemplate shipment (sortRatesByPrice shipment.rates) order.oId

-- | Step 2: buy the selected rate, persist tracking info, return success HTML.
buyLabelAndUpdate ::
  Orders.Id ->
  LabelForm ->
  ExceptT HandlerError AppM (Lucid.Html ())
buyLabelAndUpdate orderId form = do
  apiKey <-
    fromMaybeM (throwHandlerFailure "EasyPost API key is not configured.") $
      lift (asks (getter @(Maybe EasyPostApiKey)))

  manager <- lift $ asks (getter @Manager)

  let shipmentId = lfShipmentId form
      rateId = lfRateId form

  result <- liftIO $ buyShipment manager apiKey shipmentId (ShipmentBuy rateId)

  case result of
    Left (EasyPostClientError err) -> do
      lift $ Log.logInfo "EasyPost buyShipment failed" (show err)
      pure $ renderBanner Error "Label Error" (easyPostFailureMessage (EasyPostClientError err))
    Right shipment -> do
      let trackingNumber = fromMaybe "" shipment.trackingCode
          mLabelUrl = fmap (\(Label url) -> url) shipment.postageLabel
      lift $
        void $
          execQueryThrow
            (Orders.updateTracking orderId trackingNumber (Just shipmentId) mLabelUrl)
      pure $ Templates.successTemplate trackingNumber mLabelUrl

--------------------------------------------------------------------------------

-- | Resolve the total parcel weight in ounces from order items.
--
-- For each item, fetches the product and optionally the variant to determine
-- the effective weight. Variant weight takes precedence over product weight
-- when set.
computeTotalWeight ::
  [OrderItems.Model] ->
  ExceptT HandlerError AppM Double
computeTotalWeight items = do
  weights <- traverse itemWeight items
  pure $ fromIntegral (sum weights)

-- | Compute effective weight in ounces for a single order item.
--
-- Uses the variant weight when available, falling back to the product weight.
-- Multiplies by quantity.
itemWeight ::
  OrderItems.Model ->
  ExceptT HandlerError AppM Int64
itemWeight item = do
  product' <-
    fromMaybeM (throwHandlerFailure "Product not found for order item.") $
      lift (execQueryThrow (Products.getById item.oiProductId))

  effectiveWeight <- case item.oiVariantId of
    Nothing -> pure product'.pWeightOz
    Just vid -> do
      mVariant <- lift $ execQueryThrow (ProductVariants.getById vid)
      pure $ case mVariant of
        Nothing -> product'.pWeightOz
        Just variant -> fromMaybe product'.pWeightOz variant.pvWeightOz

  pure (effectiveWeight * item.oiQuantity)

-- | Build an EasyPost ShipmentCreate from the order and store settings.
buildShipmentCreate ::
  Orders.Model ->
  StoreSettings.Model ->
  Double ->
  ShipmentCreate
buildShipmentCreate order settings totalWeightOz =
  ShipmentCreate
    { fromAddress = fromAddr,
      toAddress = toAddr,
      parcel = Parcel {weight = totalWeightOz},
      verify = []
    }
  where
    fromAddr =
      Address
        { name = settings.ssShipFromName,
          street1 = settings.ssShipFromAddressLine1,
          street2 = Nothing,
          city = settings.ssShipFromCity,
          state = settings.ssShipFromState,
          zip = settings.ssShipFromZip,
          country = settings.ssShipFromCountry
        }
    toAddr =
      Address
        { name = order.oShippingFirstName <> " " <> order.oShippingLastName,
          street1 = order.oShippingAddressLine1,
          street2 =
            if Text.null order.oShippingAddressLine2
              then Nothing
              else Just order.oShippingAddressLine2,
          city = order.oShippingCity,
          state = order.oShippingState,
          zip = order.oShippingZip,
          country = order.oShippingCountry
        }
