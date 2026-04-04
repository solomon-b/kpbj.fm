-- | Stripe API client functions.
--
-- Provides high-level functions for interacting with the Stripe API.
-- Each function takes a 'Manager', a 'StripeSecretKey', and the relevant
-- parameters, returning either a 'StripeClientError' or the expected result.
--
-- Uses @servant-client@ under the hood to derive HTTP request functions
-- from the API types in "Stripe.API".
module Stripe.Client
  ( -- * Error Type
    StripeClientError (..),

    -- * Payment Intent Client Functions
    createPaymentIntent,
    getPaymentIntent,

    -- * Checkout Session Client Functions
    createCheckoutSession,
    getCheckoutSession,
  )
where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client (Manager)
import Servant.API ((:<|>) (..))
import Servant.Client qualified as Client
import Stripe.API (StripeAPI)
import Stripe.Types
  ( CheckoutSession,
    CheckoutSessionCreate,
    PaymentIntent,
    PaymentIntentCreate,
    StripeSecretKey (..),
  )

-- | An error from a Stripe API call, wrapping the underlying 'Client.ClientError'.
newtype StripeClientError = StripeClientError {unStripeClientError :: Client.ClientError}
  deriving stock (Show)

-- | Base URL for the Stripe API.
stripeBaseUrl :: Client.BaseUrl
stripeBaseUrl =
  Client.BaseUrl
    { Client.baseUrlScheme = Client.Https,
      Client.baseUrlHost = "api.stripe.com",
      Client.baseUrlPort = 443,
      Client.baseUrlPath = ""
    }

-- | Derive client functions from the 'StripeAPI' type.
createPaymentIntentClient :: Text -> PaymentIntentCreate -> Client.ClientM PaymentIntent
getPaymentIntentClient :: Text -> Text -> Client.ClientM PaymentIntent
createCheckoutSessionClient :: Text -> CheckoutSessionCreate -> Client.ClientM CheckoutSession
getCheckoutSessionClient :: Text -> Text -> Client.ClientM CheckoutSession
( createPaymentIntentClient
    :<|> getPaymentIntentClient
    :<|> createCheckoutSessionClient
    :<|> getCheckoutSessionClient
  ) = Client.client (Proxy :: Proxy StripeAPI)

-- | Format a secret key as a Bearer authorization header value.
bearerAuth :: StripeSecretKey -> Text
bearerAuth (StripeSecretKey key) = "Bearer " <> Text.decodeUtf8 key

-- | Create a new PaymentIntent.
--
-- Sends a @POST /v1/payment_intents@ request with the given parameters
-- encoded as @application/x-www-form-urlencoded@.
createPaymentIntent ::
  -- | HTTP connection manager
  Manager ->
  -- | API secret key
  StripeSecretKey ->
  -- | Payment intent parameters
  PaymentIntentCreate ->
  IO (Either StripeClientError PaymentIntent)
createPaymentIntent manager key params = do
  let env = Client.mkClientEnv manager stripeBaseUrl
  result <- Client.runClientM (createPaymentIntentClient (bearerAuth key) params) env
  pure $ case result of
    Left err -> Left (StripeClientError err)
    Right pi' -> Right pi'

-- | Retrieve an existing PaymentIntent by its identifier.
--
-- Sends a @GET /v1/payment_intents/:id@ request.
getPaymentIntent ::
  -- | HTTP connection manager
  Manager ->
  -- | API secret key
  StripeSecretKey ->
  -- | PaymentIntent identifier (e.g. @pi_1234@)
  Text ->
  IO (Either StripeClientError PaymentIntent)
getPaymentIntent manager key piId = do
  let env = Client.mkClientEnv manager stripeBaseUrl
  result <- Client.runClientM (getPaymentIntentClient (bearerAuth key) piId) env
  pure $ case result of
    Left err -> Left (StripeClientError err)
    Right pi' -> Right pi'

-- | Create a new Checkout Session.
--
-- Sends a @POST /v1/checkout/sessions@ request with the given parameters
-- encoded as @application/x-www-form-urlencoded@.
createCheckoutSession ::
  -- | HTTP connection manager
  Manager ->
  -- | API secret key
  StripeSecretKey ->
  -- | Checkout session parameters
  CheckoutSessionCreate ->
  IO (Either StripeClientError CheckoutSession)
createCheckoutSession manager key params = do
  let env = Client.mkClientEnv manager stripeBaseUrl
  result <- Client.runClientM (createCheckoutSessionClient (bearerAuth key) params) env
  pure $ case result of
    Left err -> Left (StripeClientError err)
    Right cs -> Right cs

-- | Retrieve an existing Checkout Session by its identifier.
--
-- Sends a @GET /v1/checkout/sessions/:id@ request.
getCheckoutSession ::
  -- | HTTP connection manager
  Manager ->
  -- | API secret key
  StripeSecretKey ->
  -- | Checkout Session identifier (e.g. @cs_test_...@)
  Text ->
  IO (Either StripeClientError CheckoutSession)
getCheckoutSession manager key csId = do
  let env = Client.mkClientEnv manager stripeBaseUrl
  result <- Client.runClientM (getCheckoutSessionClient (bearerAuth key) csId) env
  pure $ case result of
    Left err -> Left (StripeClientError err)
    Right cs -> Right cs
