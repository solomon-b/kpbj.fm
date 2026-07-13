-- | Servant API type definitions for the EasyPost REST API.
--
-- Defines the subset of EasyPost endpoints used by this application.
-- These types are consumed by "EasyPost.Client" to derive request functions
-- via @servant-client@.
module EasyPost.API
  ( -- * Individual Endpoints
    CreateShipmentAPI,
    GetShipmentAPI,
    BuyShipmentAPI,

    -- * Combined API
    EasyPostAPI,
  )
where

import Data.Text (Text)
import Servant.API
  ( Capture,
    Get,
    Header',
    JSON,
    Post,
    PostCreated,
    ReqBody,
    Required,
    Strict,
    (:<|>),
    (:>),
  )
import EasyPost.Types (Shipment, ShipmentBuy, ShipmentParams)


-- | @POST /v2/shipments@
--
-- Creates a new shipment with the given addresses and parcel.
-- On success, the returned 'Shipment' will contain available rate options.
type CreateShipmentAPI =
  "v2"
    :> "shipments"
    :> Header' '[Required, Strict] "Authorization" Text
    :> ReqBody '[JSON] ShipmentParams
    :> PostCreated '[JSON] Shipment


-- | @GET /v2/shipments/:id@
--
-- Retrieves an existing shipment by its identifier.
type GetShipmentAPI =
  "v2"
    :> "shipments"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Capture "id" Text
    :> Get '[JSON] Shipment


-- | @POST /v2/shipments/:id/buy@
--
-- Purchases a shipment at a specific rate. On success, the returned
-- 'Shipment' will contain a tracking code and postage label.
type BuyShipmentAPI =
  "v2"
    :> "shipments"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Capture "id" Text
    :> "buy"
    :> ReqBody '[JSON] ShipmentBuy
    :> Post '[JSON] Shipment


-- | Combined EasyPost API used to derive client functions.
type EasyPostAPI =
  CreateShipmentAPI
    :<|> GetShipmentAPI
    :<|> BuyShipmentAPI
