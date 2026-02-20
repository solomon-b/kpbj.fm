module API.Dashboard.EphemeralUploads.Id.Edit.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Id.Edit.Get.Handler (UploadEditViewData (..), action)
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
import Test.Database.Helpers (insertTestEphemeralUpload)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, nonExistentId, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.EphemeralUploads.Id.Edit.Get.Handler" $ do
      describe "action" $ do
        it "NotFound for non-existent upload" test_notFoundForNonExistentUpload
        it "returns edit data for existing upload" test_returnsEditDataForExistingUpload

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
  userInsert <- mkUserInsert "eu-edit-notfound" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (EphemeralUploads.Id nonExistentId)

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

test_returnsEditDataForExistingUpload :: TestDBConfig -> IO ()
test_returnsEditDataForExistingUpload cfg = do
  userInsert <- mkUserInsert "eu-edit-exists" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      uploadId <- insertTestEphemeralUpload (mkEphemeralUploadInsert "Edit Test Upload" userModel.mId)
      pure (userModel, userMetaModel, uploadId)
    (userModel, userMetaModel, uploadId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel uploadId

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> EphemeralUploads.eumId (uevEphemeralUpload vd) `shouldBe` uploadId
