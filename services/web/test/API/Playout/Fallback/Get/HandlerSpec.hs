module API.Playout.Fallback.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Playout.Fallback.Get.Handler (handler)
import API.Playout.Types (PlayoutTrack (..))
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Text (Text)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEphemeralUpload, insertTestStationId, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Playout.Fallback.Get.Handler" $ do
      it "returns the ephemeral track alone, with no station ID" test_ephemeralOnly
      it "returns the ephemeral track when no station ID exists" test_noStationId
      it "returns an empty array when no ephemeral exists" test_neitherExist

--------------------------------------------------------------------------------

mkEphemeralInsert :: Text -> User.Id -> EphemeralUploads.Insert
mkEphemeralInsert title creatorId =
  EphemeralUploads.Insert
    { euiTitle = title,
      euiDescription = "Test ephemeral upload",
      euiAudioFilePath = "audio/ephemeral/2026/03/15/test_2026-03-15_abc123.mp3",
      euiMimeType = "audio/mpeg",
      euiFileSize = 1024 :: Int64,
      euiCreatorId = creatorId
    }

mkStationIdInsert :: Text -> User.Id -> StationIds.Insert
mkStationIdInsert title creatorId =
  StationIds.Insert
    { siiTitle = title,
      siiAudioFilePath = "audio/station-ids/2026/03/15/test_2026-03-15_def456.mp3",
      siiMimeType = "audio/mpeg",
      siiFileSize = 512 :: Int64,
      siiDurationSeconds = Just (10 :: Int64),
      siiCreatorId = creatorId
    }

--------------------------------------------------------------------------------

-- | A station ID in the table must not reach the filler pool.
--
-- Break windows place station IDs. The fallback used to lead every ephemeral
-- track with one, and this is the test that it no longer does.
test_ephemeralOnly :: TestDBConfig -> IO ()
test_ephemeralOnly cfg = do
  userInsert <- mkUserInsert "fallback-both" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      _ <- insertTestEphemeralUpload (mkEphemeralInsert "Test Ephemeral" userId)
      _ <- insertTestStationId (mkStationIdInsert "Test Station ID" userId)
      pure ()

    liftIO $ case dbResult of
      Left err -> error $ "Setup failed: " <> show err
      Right () -> pure ()

    tracks <- handler

    liftIO $ map ptSourceType tracks `shouldBe` ["ephemeral"]

test_noStationId :: TestDBConfig -> IO ()
test_noStationId cfg = do
  userInsert <- mkUserInsert "fallback-eph-only" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      _ <- insertTestEphemeralUpload (mkEphemeralInsert "Test Ephemeral" userId)
      pure ()

    liftIO $ case dbResult of
      Left err -> error $ "Setup failed: " <> show err
      Right () -> pure ()

    tracks <- handler

    liftIO $ do
      length tracks `shouldBe` 1
      case tracks of
        (first : _) -> ptSourceType first `shouldBe` "ephemeral"
        _ -> error "Expected at least 1 track"

test_neitherExist :: TestDBConfig -> IO ()
test_neitherExist cfg =
  bracketAppM cfg $ do
    tracks <- handler
    liftIO $ tracks `shouldSatisfy` null
