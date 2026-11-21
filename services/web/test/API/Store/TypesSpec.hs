{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the JSON request types of the store.
module API.Store.TypesSpec (spec) where

--------------------------------------------------------------------------------

import API.Store.Types (CreateSessionRequest (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text.Display (display)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "API.Store.Types" $
    describe "CreateSessionRequest" $ do
      it "decodes the fields that the checkout page sends" $
        case decodeWithEmail "buyer@example.com" of
          Left err -> expectationFailure err
          Right req -> do
            display req.csrEmail `shouldBe` "buyer@example.com"
            req.csrFirstName `shouldBe` "Ada"
            req.csrLastName `shouldBe` "Lovelace"
            req.csrAddressLine1 `shouldBe` "1 Main St"
            req.csrAddressLine2 `shouldBe` ""
            req.csrCity `shouldBe` "Sun Valley"
            req.csrState `shouldBe` "CA"
            req.csrZip `shouldBe` "91352"
            req.csrShippingRateId `shouldBe` "rate_1"
            req.csrShippingShipmentId `shouldBe` "shp_1"
            length req.csrCart `shouldBe` 0

      it "refuses an email address that is not valid with the message for the user" $
        decodeWithEmail "buyer@"
          `shouldSatisfy` either ("Please enter a valid email address." `isInfixOf`) (const False)

--------------------------------------------------------------------------------
-- Helpers

-- | Decode the body that the checkout page sends, with the given email.
decodeWithEmail :: Text -> Either String CreateSessionRequest
decodeWithEmail email =
  Aeson.eitherDecode $
    Aeson.encode $
      Aeson.object
        [ "email" .= email,
          "first_name" .= ("Ada" :: Text),
          "last_name" .= ("Lovelace" :: Text),
          "address_line1" .= ("1 Main St" :: Text),
          "address_line2" .= ("" :: Text),
          "city" .= ("Sun Valley" :: Text),
          "state" .= ("CA" :: Text),
          "zip" .= ("91352" :: Text),
          "shipping_rate_id" .= ("rate_1" :: Text),
          "shipping_shipment_id" .= ("shp_1" :: Text),
          "cart" .= ([] :: [Aeson.Value])
        ]
