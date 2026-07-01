{-# LANGUAGE OverloadedRecordDot #-}

-- | Handler for POST /api/store/shipping-rates.
--
-- Receives a shipping address and cart, calls EasyPost to obtain rate
-- quotes, and returns an HTML fragment for HTMX injection into
-- @#shipping-rates@.
module API.Store.ShippingRates.Post.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import API.Store.ShippingRates.Post.Templates qualified as Templates
import API.Store.Types (CartItem (..), ShippingRateRequest (..))
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Has (getter)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cents (Cents (..))
import EasyPost.Client (EasyPostClientError (..), createShipment)
import EasyPost.Types
  ( Address (..),
    EasyPostApiKey,
    Parcel (..),
    Shipment (..),
    ShipmentCreate (..),
  )
import Effects.Database.Execute (execQueryThrow)
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.StoreSettings qualified as StoreSettings
import Log qualified
import Lucid qualified
import Network.HTTP.Client (Manager)
import Store.Checkout.Logic (computeSubtotal, sortRatesByPrice)
import Store.Checkout.ShippingErrors (deliveryVerificationError, easyPostFailureMessage)

--------------------------------------------------------------------------------

-- | Handler for shipping rate requests.
--
-- Validates the cart, fetches product data to compute parcel weight,
-- loads store settings for the ship-from address, then calls EasyPost.
-- Returns an HTML fragment with rate options on success, or an error
-- banner on failure.
handler ::
  ShippingRateRequest ->
  AppM (Lucid.Html ())
handler req = do
  let cart = take 50 req.srrCart
  case cart of
    [] -> pure $ renderBanner Error "Empty Cart" "Your cart is empty."
    _ -> do
      mResult <- resolveCartItems cart
      case mResult of
        Left msg -> pure $ renderBanner Error "Cart Error" msg
        Right resolvedItems -> do
          mSettings <- execQueryThrow StoreSettings.getSettings
          case mSettings of
            Nothing -> pure $ renderBanner Error "Configuration Error" "Store settings are not configured."
            Just settings -> do
              mEasyPostKey <- asks (getter @(Maybe EasyPostApiKey))
              case mEasyPostKey of
                Nothing -> pure $ renderBanner Error "Shipping Unavailable" "Shipping rate lookup is not configured."
                Just apiKey -> do
                  manager <- asks (getter @Manager)
                  let shipmentCreate = buildShipmentCreate req settings resolvedItems
                  result <- liftIO $ createShipment manager apiKey shipmentCreate
                  case result of
                    Left (EasyPostClientError err) -> do
                      Log.logInfo "EasyPost createShipment failed" (show err)
                      pure $ renderBanner Error "Address Problem" (easyPostFailureMessage (EasyPostClientError err))
                    Right shipment ->
                      case deliveryVerificationError shipment of
                        Just msg -> pure $ renderBanner Error "Address Problem" msg
                        Nothing -> do
                          let subtotal =
                                computeSubtotal
                                  [ (ri.riUnitPrice, fromIntegral ri.riQuantity)
                                  | ri <- resolvedItems
                                  ]
                              taxRate = settings.ssTaxRate
                              sortedRates = sortRatesByPrice shipment.rates
                          pure $ Templates.template shipment sortedRates subtotal taxRate

--------------------------------------------------------------------------------

-- | A cart item resolved against the database.
data ResolvedItem = ResolvedItem
  { riProductName :: !Text,
    riVariantLabel :: !(Maybe Text),
    riQuantity :: !Int,
    riUnitPrice :: !Cents,
    riWeightOz :: !Int64
  }

--------------------------------------------------------------------------------

-- | Resolve all cart items against the database.
--
-- Returns 'Left' with an error message if any item cannot be resolved.
-- Returns 'Right' with the list of resolved items otherwise.
resolveCartItems ::
  [CartItem] ->
  AppM (Either Text [ResolvedItem])
resolveCartItems items = do
  results <- traverse resolveOne items
  pure $ sequence results

resolveOne ::
  CartItem ->
  AppM (Either Text ResolvedItem)
resolveOne cartItem = do
  if cartItem.quantity <= 0 || cartItem.quantity > 50
    then pure $ Left "Invalid item quantity."
    else resolveOneValid cartItem

resolveOneValid ::
  CartItem ->
  AppM (Either Text ResolvedItem)
resolveOneValid cartItem = do
  mProduct <- execQueryThrow $ Products.getById cartItem.productId
  case mProduct of
    Nothing ->
      pure $ Left "One or more items in your cart are no longer available."
    Just product' ->
      let productName = product'.pName
       in if not product'.pIsActive
            then pure $ Left (productName <> " is no longer available.")
            else case cartItem.variantId of
              Nothing ->
                pure $
                  Right
                    ResolvedItem
                      { riProductName = productName,
                        riVariantLabel = Nothing,
                        riQuantity = cartItem.quantity,
                        riUnitPrice = product'.pBasePriceCents,
                        riWeightOz = product'.pWeightOz
                      }
              Just vid -> resolveVariant product' vid cartItem.quantity

resolveVariant ::
  Products.Model ->
  ProductVariants.Id ->
  Int ->
  AppM (Either Text ResolvedItem)
resolveVariant product' variantId qty = do
  let productName = product'.pName
  mVariant <- execQueryThrow $ ProductVariants.getById variantId
  case mVariant of
    Nothing ->
      pure $ Left ("A selected option for " <> productName <> " is no longer available.")
    Just variant ->
      let variantLabel = variant.pvLabel
       in if variant.pvProductId /= product'.pId
            then pure $ Left ("A selected option for " <> productName <> " is no longer available.")
            else
              if isJust variant.pvDeletedAt
                then pure $ Left ("The " <> variantLabel <> " option for " <> productName <> " is no longer available.")
                else
                  pure $
                    Right
                      ResolvedItem
                        { riProductName = productName,
                          riVariantLabel = Just variantLabel,
                          riQuantity = qty,
                          riUnitPrice = fromMaybe product'.pBasePriceCents variant.pvPriceCents,
                          riWeightOz = fromMaybe product'.pWeightOz variant.pvWeightOz
                        }

--------------------------------------------------------------------------------

-- | Build the EasyPost ShipmentCreate from request and resolved items.
buildShipmentCreate ::
  ShippingRateRequest ->
  StoreSettings.Model ->
  [ResolvedItem] ->
  ShipmentCreate
buildShipmentCreate req settings resolvedItems =
  ShipmentCreate
    { fromAddress = fromAddr,
      toAddress = toAddr,
      parcel = Parcel {weight = totalWeightOz},
      verify = ["delivery"]
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
        { name = req.srrFirstName <> " " <> req.srrLastName,
          street1 = req.srrAddressLine1,
          street2 = if Text.null req.srrAddressLine2 then Nothing else Just req.srrAddressLine2,
          city = req.srrCity,
          state = req.srrState,
          zip = req.srrZip,
          country = "US"
        }
    totalWeightOz :: Double
    totalWeightOz =
      fromIntegral $
        sum
          [ ri.riWeightOz * fromIntegral ri.riQuantity
          | ri <- resolvedItems
          ]

--------------------------------------------------------------------------------
