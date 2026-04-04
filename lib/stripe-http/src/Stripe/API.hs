-- | Servant API type definitions for the Stripe REST API.
--
-- Defines the subset of Stripe endpoints used by this application.
-- These types are consumed by "Stripe.Client" to derive request functions
-- via @servant-client@.
module Stripe.API
  ( -- * Payment Intent Endpoints
    CreatePaymentIntentAPI,
    GetPaymentIntentAPI,

    -- * Checkout Session Endpoints
    CreateCheckoutSessionAPI,
    GetCheckoutSessionAPI,

    -- * Combined API
    StripeAPI,
  )
where

import Data.Text (Text)
import Servant.API
  ( Capture,
    FormUrlEncoded,
    Get,
    Header',
    JSON,
    Post,
    ReqBody,
    Required,
    Strict,
    (:<|>),
    (:>),
  )
import Stripe.Types
  ( CheckoutSession,
    CheckoutSessionCreate,
    PaymentIntent,
    PaymentIntentCreate,
  )

-- | @POST /v1/payment_intents@
--
-- Creates a new PaymentIntent. Requires a Bearer token in the
-- @Authorization@ header and a form-encoded body.
type CreatePaymentIntentAPI =
  "v1"
    :> "payment_intents"
    :> Header' '[Required, Strict] "Authorization" Text
    :> ReqBody '[FormUrlEncoded] PaymentIntentCreate
    :> Post '[JSON] PaymentIntent

-- | @GET /v1/payment_intents/:id@
--
-- Retrieves an existing PaymentIntent by its identifier.
type GetPaymentIntentAPI =
  "v1"
    :> "payment_intents"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Capture "id" Text
    :> Get '[JSON] PaymentIntent

-- | @POST /v1/checkout/sessions@
--
-- Creates a new Checkout Session. Requires a Bearer token in the
-- @Authorization@ header and a form-encoded body.
type CreateCheckoutSessionAPI =
  "v1"
    :> "checkout"
    :> "sessions"
    :> Header' '[Required, Strict] "Authorization" Text
    :> ReqBody '[FormUrlEncoded] CheckoutSessionCreate
    :> Post '[JSON] CheckoutSession

-- | @GET /v1/checkout/sessions/:id@
--
-- Retrieves an existing Checkout Session by its identifier.
type GetCheckoutSessionAPI =
  "v1"
    :> "checkout"
    :> "sessions"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Capture "id" Text
    :> Get '[JSON] CheckoutSession

-- | Combined Stripe API used to derive client functions.
type StripeAPI =
  CreatePaymentIntentAPI
    :<|> GetPaymentIntentAPI
    :<|> CreateCheckoutSessionAPI
    :<|> GetCheckoutSessionAPI
