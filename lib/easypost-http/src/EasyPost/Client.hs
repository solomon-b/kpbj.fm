-- | EasyPost API client functions.
--
-- Provides high-level functions for interacting with the EasyPost shipping
-- API v2. Each function takes a 'Manager', an 'EasyPostApiKey', and the
-- relevant parameters, returning either an 'EasyPostClientError' or the
-- expected result.
--
-- Uses @servant-client@ under the hood to derive HTTP request functions
-- from the API types in "EasyPost.API".
module EasyPost.Client
  ( -- * Error Type
    EasyPostClientError (..),

    -- * Client Functions
    createShipment,
    getShipment,
    buyShipment,
  )
where

import Data.ByteString.Base64 qualified as Base64
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import EasyPost.API (EasyPostAPI)
import EasyPost.Types
  ( EasyPostApiKey (..),
    Shipment,
    ShipmentBuy,
    ShipmentCreate,
  )
import Network.HTTP.Client (Manager)
import Servant.API ((:<|>) (..))
import Servant.Client qualified as Client


-- | An error from an EasyPost API call, wrapping the underlying 'Client.ClientError'.
newtype EasyPostClientError = EasyPostClientError {unEasyPostClientError :: Client.ClientError}
  deriving stock (Show)


-- | Base URL for the EasyPost API.
easyPostBaseUrl :: Client.BaseUrl
easyPostBaseUrl =
  Client.BaseUrl
    { Client.baseUrlScheme = Client.Https,
      Client.baseUrlHost = "api.easypost.com",
      Client.baseUrlPort = 443,
      Client.baseUrlPath = ""
    }


-- | Derive client functions from the 'EasyPostAPI' type.
createShipmentClient :: Text -> ShipmentCreate -> Client.ClientM Shipment
getShipmentClient :: Text -> Text -> Client.ClientM Shipment
buyShipmentClient :: Text -> Text -> ShipmentBuy -> Client.ClientM Shipment
createShipmentClient :<|> getShipmentClient :<|> buyShipmentClient = Client.client (Proxy :: Proxy EasyPostAPI)


-- | Build an HTTP Basic @Authorization@ header value.
--
-- EasyPost uses the API key as the username with an empty password,
-- encoded as @base64("apikey:")@.
mkAuthHeader :: EasyPostApiKey -> Text
mkAuthHeader (EasyPostApiKey key) =
  "Basic " <> Text.decodeUtf8 (Base64.encode (key <> ":"))


-- | Create a new shipment with the given addresses and parcel.
--
-- Sends a @POST /v2/shipments@ request. On success, the returned
-- 'Shipment' will contain available 'EasyPost.Types.Rate' options.
createShipment ::
  -- | HTTP connection manager (use 'Network.HTTP.Client.TLS.newTlsManager' to create one)
  Manager ->
  -- | API key
  EasyPostApiKey ->
  -- | Shipment parameters
  ShipmentCreate ->
  IO (Either EasyPostClientError Shipment)
createShipment manager apiKey params = do
  let env = Client.mkClientEnv manager easyPostBaseUrl
  result <- Client.runClientM (createShipmentClient (mkAuthHeader apiKey) params) env
  pure $ case result of
    Left err -> Left (EasyPostClientError err)
    Right shipment -> Right shipment


-- | Retrieve an existing shipment by its identifier.
--
-- Sends a @GET /v2/shipments/:id@ request.
getShipment ::
  -- | HTTP connection manager
  Manager ->
  -- | API key
  EasyPostApiKey ->
  -- | Shipment identifier (e.g. @"shp_abc123"@)
  Text ->
  IO (Either EasyPostClientError Shipment)
getShipment manager apiKey shipmentId = do
  let env = Client.mkClientEnv manager easyPostBaseUrl
  result <- Client.runClientM (getShipmentClient (mkAuthHeader apiKey) shipmentId) env
  pure $ case result of
    Left err -> Left (EasyPostClientError err)
    Right shipment -> Right shipment


-- | Purchase a shipment at the specified rate.
--
-- Sends a @POST /v2/shipments/:id/buy@ request. On success, the returned
-- 'Shipment' will contain a 'EasyPost.Types.trackingCode' and
-- 'EasyPost.Types.postageLabel'.
buyShipment ::
  -- | HTTP connection manager
  Manager ->
  -- | API key
  EasyPostApiKey ->
  -- | Shipment identifier (e.g. @"shp_abc123"@)
  Text ->
  -- | Purchase parameters
  ShipmentBuy ->
  IO (Either EasyPostClientError Shipment)
buyShipment manager apiKey shipmentId buy = do
  let env = Client.mkClientEnv manager easyPostBaseUrl
  result <- Client.runClientM (buyShipmentClient (mkAuthHeader apiKey) shipmentId buy) env
  pure $ case result of
    Left err -> Left (EasyPostClientError err)
    Right shipment -> Right shipment
