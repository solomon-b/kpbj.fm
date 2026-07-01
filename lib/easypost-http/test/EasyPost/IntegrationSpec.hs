-- | Integration test against the live EasyPost test API.
--
-- Requires EASYPOST_API_KEY env var set to a test key.
-- Skipped automatically if the env var is missing.
module EasyPost.IntegrationSpec where

import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import EasyPost.Client (buyShipment, createShipment, getShipment)
import EasyPost.Types
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, describe, it, pendingWith, shouldBe, shouldSatisfy)

-- | Helper to get EasyPost key and manager, or skip the test.
withEasyPost :: (EasyPostApiKey -> Manager -> IO ()) -> IO ()
withEasyPost action = do
  mKey <- lookupEnv "EASYPOST_API_KEY"
  case mKey of
    Nothing -> pendingWith "EASYPOST_API_KEY not set, skipping integration test"
    Just keyStr -> do
      let key = EasyPostApiKey (TE.encodeUtf8 (Text.pack keyStr))
      mgr <- newTlsManager
      action key mgr

-- | Test addresses using EasyPost's recommended test values.
fromAddress :: Address
fromAddress =
  Address
    { name = "KPBJ 95.9FM",
      street1 = "417 Montgomery Street",
      street2 = Just "Floor 5",
      city = "San Francisco",
      state = "CA",
      zip = "94104",
      country = "US"
    }

toAddress :: Address
toAddress =
  Address
    { name = "Jane Doe",
      street1 = "388 Townsend St",
      street2 = Just "Apt 20",
      city = "San Francisco",
      state = "CA",
      zip = "94107",
      country = "US"
    }

spec :: Spec
spec = describe "EasyPost Integration (live test API)" $ do
  it "creates a shipment and gets rates" $ withEasyPost $ \key mgr -> do
    let sc =
          ShipmentCreate
            { fromAddress = fromAddress,
              toAddress = toAddress,
              parcel = Parcel {weight = 16.0},
              verify = []
            }

    createResult <- createShipment mgr key sc
    case createResult of
      Left err -> fail $ "Failed to create shipment: " <> show err
      Right shipment -> do
        -- Shipment should have an ID
        shipment.id `shouldSatisfy` (not . Text.null)
        -- Should have at least one rate
        shipment.rates `shouldSatisfy` (not . null)
        -- Each rate should have carrier and service
        mapM_ (\r -> do
          r.carrier `shouldSatisfy` (not . Text.null)
          r.service `shouldSatisfy` (not . Text.null)
          r.rate `shouldSatisfy` (not . Text.null)
          ) shipment.rates

  it "retrieves a shipment by ID" $ withEasyPost $ \key mgr -> do
    let sc =
          ShipmentCreate
            { fromAddress = fromAddress,
              toAddress = toAddress,
              parcel = Parcel {weight = 8.0},
              verify = []
            }

    createResult <- createShipment mgr key sc
    case createResult of
      Left err -> fail $ "Failed to create shipment: " <> show err
      Right shipment -> do
        retrieveResult <- getShipment mgr key shipment.id
        case retrieveResult of
          Left err -> fail $ "Failed to retrieve shipment: " <> show err
          Right shipment' -> do
            shipment'.id `shouldBe` shipment.id

  it "buys a shipment and gets tracking + label" $ withEasyPost $ \key mgr -> do
    let sc =
          ShipmentCreate
            { fromAddress = fromAddress,
              toAddress = toAddress,
              parcel = Parcel {weight = 10.0},
              verify = []
            }

    createResult <- createShipment mgr key sc
    case createResult of
      Left err -> fail $ "Failed to create shipment: " <> show err
      Right shipment -> do
        -- Pick the first rate
        case shipment.rates of
          [] -> fail "No rates returned"
          (cheapest : _) -> do
            let buy = ShipmentBuy {rateId = cheapest.id}
            buyResult <- buyShipment mgr key shipment.id buy
            case buyResult of
              Left err -> fail $ "Failed to buy shipment: " <> show err
              Right bought -> do
                bought.id `shouldBe` shipment.id
                bought.trackingCode `shouldSatisfy` \case
                  Just tc -> not (Text.null tc)
                  Nothing -> False
                bought.postageLabel `shouldSatisfy` \case
                  Just label -> not (Text.null label.labelUrl)
                  Nothing -> False
