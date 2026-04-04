-- | Integration test against the live Stripe test API.
--
-- Requires STRIPE_SECRET_KEY env var set to a sk_test_... key.
-- Skipped automatically if the env var is missing.
module Stripe.IntegrationSpec where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Stripe.Client (createCheckoutSession, createPaymentIntent, getCheckoutSession, getPaymentIntent)
import Stripe.Types
import System.Environment (lookupEnv)
import Test.Hspec (Spec, describe, it, pendingWith, shouldBe, shouldSatisfy)

-- | Helper to get Stripe key and manager, or skip the test.
withStripe :: (StripeSecretKey -> Manager -> IO ()) -> IO ()
withStripe action = do
  mKey <- lookupEnv "STRIPE_SECRET_KEY"
  case mKey of
    Nothing -> pendingWith "STRIPE_SECRET_KEY not set, skipping integration test"
    Just keyStr -> do
      let key = StripeSecretKey (TE.encodeUtf8 (Text.pack keyStr))
      mgr <- newTlsManager
      action key mgr

spec :: Spec
spec = describe "Stripe Integration (live test API)" $ do
  describe "Checkout Sessions" $ do
    it "creates and retrieves a Checkout Session" $ withStripe $ \key mgr -> do
      let session =
            CheckoutSessionCreate
              { mode = "payment",
                uiMode = "custom",
                returnUrl = "https://example.com/return?session_id={CHECKOUT_SESSION_ID}",
                customerEmail = Just "test@example.com",
                lineItems =
                  [ LineItem
                      { priceData =
                          PriceData
                            { currency = USD,
                              unitAmount = 2000,
                              productData =
                                ProductData
                                  { name = "Test Product",
                                    description = Just "Integration test item"
                                  }
                            },
                        quantity = 1
                      }
                  ],
                shippingOptions = [],
                paymentMethodTypes = ["card"],
                metadata = Map.singleton "test" "true"
              }

      createResult <- createCheckoutSession mgr key session
      case createResult of
        Left err -> fail $ "Failed to create Checkout Session: " <> show err
        Right cs -> do
          cs.status `shouldBe` Open
          cs.paymentStatus `shouldBe` Unpaid
          cs.clientSecret `shouldSatisfy` \case
            Just s -> not (Text.null s)
            Nothing -> False

          retrieveResult <- getCheckoutSession mgr key cs.id
          case retrieveResult of
            Left err -> fail $ "Failed to retrieve Checkout Session: " <> show err
            Right cs' -> do
              cs'.id `shouldBe` cs.id
              cs'.status `shouldBe` Open

    it "creates a Checkout Session with shipping options" $ withStripe $ \key mgr -> do
      let session =
            CheckoutSessionCreate
              { mode = "payment",
                uiMode = "custom",
                returnUrl = "https://example.com/return?session_id={CHECKOUT_SESSION_ID}",
                customerEmail = Just "test@example.com",
                lineItems =
                  [ LineItem
                      { priceData =
                          PriceData
                            { currency = USD,
                              unitAmount = 2500,
                              productData =
                                ProductData
                                  { name = "KPBJ Logo Tee",
                                    description = Just "Size M"
                                  }
                            },
                        quantity = 1
                      }
                  ],
                shippingOptions =
                  [ ShippingOption
                      { shippingRateData =
                          ShippingRateData
                            { type_ = "fixed_amount",
                              displayName = "USPS Priority Mail (2-3 days)",
                              fixedAmount = FixedAmount {amount = 899, currency = USD}
                            }
                      }
                  ],
                paymentMethodTypes = ["card"],
                metadata = Map.singleton "order_number" "KPBJ-20260402-TEST"
              }

      createResult <- createCheckoutSession mgr key session
      case createResult of
        Left err -> fail $ "Failed to create Checkout Session with shipping: " <> show err
        Right cs -> do
          cs.status `shouldBe` Open
          cs.clientSecret `shouldSatisfy` \case
            Just s -> not (Text.null s)
            Nothing -> False

  describe "Payment Intents" $ do
    it "creates and retrieves a PaymentIntent" $ withStripe $ \key mgr -> do
      let pic =
            PaymentIntentCreate
              { amount = 1500,
                currency = USD,
                metadata = Map.singleton "test" "true"
              }

      createResult <- createPaymentIntent mgr key pic
      case createResult of
        Left err -> fail $ "Failed to create PaymentIntent: " <> show err
        Right pi' -> do
          pi'.amount `shouldBe` 1500
          pi'.currency `shouldBe` USD
          pi'.status `shouldBe` RequiresPaymentMethod
          pi'.clientSecret `shouldSatisfy` \case
            Just s -> not (Text.null s)
            Nothing -> False

          retrieveResult <- getPaymentIntent mgr key pi'.id
          case retrieveResult of
            Left err -> fail $ "Failed to retrieve PaymentIntent: " <> show err
            Right pi'' -> do
              pi''.id `shouldBe` pi'.id
              pi''.amount `shouldBe` 1500
              pi''.status `shouldBe` RequiresPaymentMethod
