-- | Pure JSON decoding tests for EasyPost error and verification types.
module EasyPost.TypesSpec where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import EasyPost.Types
  ( EasyPostError (..),
    EasyPostFieldError (..),
    Shipment (..),
    Verification (..),
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "EasyPostError FromJSON" $ do
    it "decodes a PARAMETER.INVALID_TYPE (weight) error and unwraps the envelope" $ do
      let body :: ByteString
          body =
            "{\"error\":{\"code\":\"PARAMETER.INVALID_TYPE\",\"message\":\"Wrong parameter type.\",\"errors\":[{\"field\":\"shipment.parcel.weight\",\"message\":\"must be greater than 0\"}]}}"
      Aeson.decode body
        `shouldBe` Just
          EasyPostError
            { code = "PARAMETER.INVALID_TYPE",
              message = "Wrong parameter type.",
              errors =
                [ EasyPostFieldError
                    { field = Just "shipment.parcel.weight",
                      message = "must be greater than 0",
                      suggestion = Nothing
                    }
                ]
            }

    it "decodes an ADDRESS.VERIFY.FAILURE error with a null suggestion" $ do
      let body :: ByteString
          body =
            "{\"error\":{\"code\":\"ADDRESS.VERIFY.FAILURE\",\"message\":\"Unable to verify address.\",\"errors\":[{\"field\":\"street1\",\"message\":\"House number is missing\",\"suggestion\":null}]}}"
      Aeson.decode body
        `shouldBe` Just
          EasyPostError
            { code = "ADDRESS.VERIFY.FAILURE",
              message = "Unable to verify address.",
              errors =
                [ EasyPostFieldError
                    { field = Just "street1",
                      message = "House number is missing",
                      suggestion = Nothing
                    }
                ]
            }

    it "tolerates a bare-string errors[] item" $ do
      let body :: ByteString
          body =
            "{\"error\":{\"code\":\"UNKNOWN\",\"message\":\"Something went wrong.\",\"errors\":[\"a bare string error\"]}}"
      Aeson.decode body
        `shouldBe` Just
          EasyPostError
            { code = "UNKNOWN",
              message = "Something went wrong.",
              errors =
                [ EasyPostFieldError
                    { field = Nothing,
                      message = "a bare string error",
                      suggestion = Nothing
                    }
                ]
            }

  describe "Shipment FromJSON delivery verification" $ do
    it "decodes a failed to_address delivery verification" $ do
      let body :: ByteString
          body =
            "{\"id\":\"shp_1\",\"rates\":[],\"to_address\":{\"verifications\":{\"delivery\":{\"success\":false,\"errors\":[{\"code\":\"E.ADDRESS.NOT_FOUND\",\"field\":\"address\",\"message\":\"Address not found\",\"suggestion\":null}],\"details\":null}}}}"
          mShipment = Aeson.decode body :: Maybe Shipment
      fmap (.toAddressDeliveryVerification) mShipment
        `shouldBe` Just
          ( Just
              Verification
                { success = False,
                  errors =
                    [ EasyPostFieldError
                        { field = Just "address",
                          message = "Address not found",
                          suggestion = Nothing
                        }
                    ]
                }
          )

    it "yields Nothing when verifications are absent" $ do
      let body :: ByteString
          body = "{\"id\":\"shp_2\",\"rates\":[]}"
          mShipment = Aeson.decode body :: Maybe Shipment
      fmap (.toAddressDeliveryVerification) mShipment
        `shouldBe` Just Nothing
