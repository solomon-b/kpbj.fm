module Store.Checkout.ShippingErrorsSpec (spec) where

--------------------------------------------------------------------------------

import Control.Exception (ErrorCall (..), toException)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import EasyPost.Client (EasyPostClientError (..))
import EasyPost.Types
  ( Address (..),
    EasyPostFieldError (..),
    Parcel (..),
    Shipment (..),
    Verification (..),
    Verifications (..),
  )
import Network.HTTP.Types (http11, status422)
import Servant.Client.Core qualified as Client
import Store.Checkout.ShippingErrors (deliveryVerificationError, easyPostFailureMessage)
import Test.Hspec

--------------------------------------------------------------------------------

-- | Wrap a JSON body in a 'Client.DecodeFailure', the simplest 'Client.ClientError'
-- carrying a response body.
mkClientError :: ByteString -> EasyPostClientError
mkClientError body =
  EasyPostClientError $
    Client.DecodeFailure "test" $
      Client.Response status422 mempty http11 body

-- | A connection-level error, which carries no response body.
connectionError :: EasyPostClientError
connectionError =
  EasyPostClientError (Client.ConnectionError (toException (ErrorCall "boom")))

genericFallback :: Text
genericFallback = "We couldn't process your shipping address. Please check it and try again."

t0 :: UTCTime
t0 = read "2025-01-01 00:00:00 UTC"

baseAddress :: Address
baseAddress =
  Address
    { id = "adr_test",
      object = "Address",
      mode = "test",
      createdAt = t0,
      updatedAt = t0,
      street1 = Just "388 Townsend St",
      street2 = Nothing,
      city = Just "San Francisco",
      state = Just "CA",
      zip = Just "94107",
      country = Just "US",
      name = Just "Jane Doe",
      company = Nothing,
      phone = Nothing,
      email = Nothing,
      federalTaxId = Nothing,
      stateTaxId = Nothing,
      residential = Nothing,
      carrierFacility = Nothing,
      verifications = Nothing
    }

baseParcel :: Parcel
baseParcel =
  Parcel
    { id = "prcl_test",
      object = "Parcel",
      mode = "test",
      createdAt = t0,
      updatedAt = t0,
      length = Nothing,
      width = Nothing,
      height = Nothing,
      weight = 16,
      predefinedPackage = Nothing
    }

baseShipment :: Shipment
baseShipment =
  Shipment
    { id = "shp_test",
      object = "Shipment",
      mode = "test",
      createdAt = t0,
      updatedAt = t0,
      reference = Nothing,
      toAddress = baseAddress,
      fromAddress = baseAddress,
      returnAddress = Nothing,
      buyerAddress = Nothing,
      parcel = baseParcel,
      customsInfo = Nothing,
      rates = [],
      selectedRate = Nothing,
      postageLabel = Nothing,
      trackingCode = Nothing,
      tracker = Nothing,
      options = Nothing,
      messages = [],
      status = Nothing,
      refundStatus = Nothing,
      isReturn = False,
      insurance = Nothing,
      uspsZone = Nothing,
      batchId = Nothing,
      batchStatus = Nothing,
      batchMessage = Nothing,
      forms = [],
      fees = [],
      orderId = Nothing,
      scanForm = Nothing,
      lineItems = Nothing
    }

-- | Build a shipment carrying only a delivery verification result.
mkShipment :: Maybe Verification -> Shipment
mkShipment verification =
  baseShipment
    { toAddress =
        baseAddress
          { verifications = Just Verifications {delivery = verification, zip4 = Nothing}
          }
    }

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "easyPostFailureMessage" $ do
    it "formats a field error with its suggestion" $ do
      let body =
            "{\"error\":{\"code\":\"ADDRESS.VERIFY.FAILURE\",\"message\":\"Unable to verify address.\",\"errors\":[{\"field\":\"street1\",\"message\":\"House number is missing\",\"suggestion\":\"123 Main St\"}]}}"
      easyPostFailureMessage (mkClientError body)
        `shouldBe` "House number is missing (suggestion: 123 Main St)"

    it "joins multiple field errors" $ do
      let body =
            "{\"error\":{\"code\":\"X\",\"message\":\"top\",\"errors\":[{\"field\":\"a\",\"message\":\"first problem\",\"suggestion\":null},{\"field\":\"b\",\"message\":\"second problem\",\"suggestion\":null}]}}"
      easyPostFailureMessage (mkClientError body)
        `shouldBe` "first problem; second problem"

    it "falls back to the top-level message when there are no field errors" $ do
      let body =
            "{\"error\":{\"code\":\"X\",\"message\":\"Top level message.\",\"errors\":[]}}"
      easyPostFailureMessage (mkClientError body)
        `shouldBe` "Top level message."

    it "uses the generic fallback for an unparseable body" $ do
      easyPostFailureMessage (mkClientError "not json at all")
        `shouldBe` genericFallback

    it "uses the generic fallback for a connection error" $ do
      easyPostFailureMessage connectionError
        `shouldBe` genericFallback

  describe "deliveryVerificationError" $ do
    it "formats errors from a failed delivery verification" $ do
      let verification =
            Verification
              { success = False,
                errors =
                  [ EasyPostFieldError
                      { field = Just "address",
                        message = "Address not found",
                        suggestion = Nothing,
                        code = Nothing
                      }
                  ],
                details = Nothing
              }
      deliveryVerificationError (mkShipment (Just verification))
        `shouldBe` Just "Address not found"

    it "returns Nothing for a successful verification" $ do
      let verification = Verification {success = True, errors = [], details = Nothing}
      deliveryVerificationError (mkShipment (Just verification))
        `shouldBe` Nothing

    it "returns Nothing when there is no verification block" $
      deliveryVerificationError (mkShipment Nothing) `shouldBe` Nothing
