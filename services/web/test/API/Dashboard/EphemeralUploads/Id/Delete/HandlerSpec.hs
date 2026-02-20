module API.Dashboard.EphemeralUploads.Id.Delete.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Id.Delete.Handler (action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int64)
import Data.Text (Text)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEphemeralUpload, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, nonExistentId, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.EphemeralUploads.Id.Delete.Handler" $ do
      describe "action" $ do
        it "NotFound for non-existent upload" test_notFoundForNonExistentUpload
        it "NotAuthorized for non-creator non-staff" test_notAuthorizedForNonCreatorNonStaff

--------------------------------------------------------------------------------

mkEphemeralUploadInsert :: Text -> User.Id -> EphemeralUploads.Insert
mkEphemeralUploadInsert title creatorId =
  EphemeralUploads.Insert
    { euiTitle = title,
      euiDescription = "Test description",
      euiAudioFilePath = "/test/audio.mp3",
      euiMimeType = "audio/mpeg",
      euiFileSize = 1024 :: Int64,
      euiCreatorId = creatorId
    }

--------------------------------------------------------------------------------

test_notFoundForNonExistentUpload :: TestDBConfig -> IO ()
test_notFoundForNonExistentUpload cfg = do
  userInsert <- mkUserInsert "eu-del-notfound" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (EphemeralUploads.Id nonExistentId)

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

test_notAuthorizedForNonCreatorNonStaff :: TestDBConfig -> IO ()
test_notAuthorizedForNonCreatorNonStaff cfg = do
  creatorInsert <- mkUserInsert "eu-del-creator" UserMetadata.Host
  otherInsert <- mkUserInsert "eu-del-other" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      creatorId <- insertTestUser creatorInsert
      (otherModel, otherMetaModel) <- setupUserModels otherInsert
      uploadId <- insertTestEphemeralUpload (mkEphemeralUploadInsert "Delete Auth Test Upload" creatorId)
      pure (otherModel, otherMetaModel, uploadId)
    (otherModel, otherMetaModel, uploadId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action otherModel otherMetaModel uploadId

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotAuthorized but got Right"
