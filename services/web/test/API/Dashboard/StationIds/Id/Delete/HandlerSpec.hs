module API.Dashboard.StationIds.Id.Delete.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.StationIds.Id.Delete.Handler (action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int64)
import Data.Text (Text)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestStationId, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, nonExistentId, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.StationIds.Id.Delete.Handler" $ do
      describe "action" $ do
        it "NotFound for non-existent station ID" test_notFoundForNonExistentStationId
        it "NotAuthorized for non-creator non-staff" test_notAuthorizedForNonCreatorNonStaff
        it "creator can delete own station ID" test_creatorCanDeleteOwnStationId

--------------------------------------------------------------------------------

mkStationIdInsert :: Text -> User.Id -> StationIds.Insert
mkStationIdInsert title creatorId =
  StationIds.Insert
    { siiTitle = title,
      siiAudioFilePath = "/test/station-id.mp3",
      siiMimeType = "audio/mpeg",
      siiFileSize = 512 :: Int64,
      siiCreatorId = creatorId
    }

--------------------------------------------------------------------------------

test_notFoundForNonExistentStationId :: TestDBConfig -> IO ()
test_notFoundForNonExistentStationId cfg = do
  userInsert <- mkUserInsert "si-del-notfound" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (StationIds.Id nonExistentId)

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

test_notAuthorizedForNonCreatorNonStaff :: TestDBConfig -> IO ()
test_notAuthorizedForNonCreatorNonStaff cfg = do
  creatorInsert <- mkUserInsert "si-del-creator" UserMetadata.Host
  otherInsert <- mkUserInsert "si-del-other" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      creatorId <- insertTestUser creatorInsert
      (otherModel, otherMetaModel) <- setupUserModels otherInsert
      stationIdId <- insertTestStationId (mkStationIdInsert "Delete Auth Test Station ID" creatorId)
      pure (otherModel, otherMetaModel, stationIdId)
    (otherModel, otherMetaModel, stationIdId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action otherModel otherMetaModel stationIdId

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotAuthorized but got Right"

test_creatorCanDeleteOwnStationId :: TestDBConfig -> IO ()
test_creatorCanDeleteOwnStationId cfg = do
  userInsert <- mkUserInsert "si-del-own" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      stationIdId <- insertTestStationId (mkStationIdInsert "Creator Delete Test Station ID" userModel.mId)
      pure (userModel, userMetaModel, stationIdId)
    (userModel, userMetaModel, stationIdId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel stationIdId

    liftIO $ case result of
      Right _ -> pure ()
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
