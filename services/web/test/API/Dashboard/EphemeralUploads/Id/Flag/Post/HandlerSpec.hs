module API.Dashboard.EphemeralUploads.Id.Flag.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Id.Flag.Post.Handler (FlagResult (..), UploadFlagViewData (..), action)
import API.Dashboard.EphemeralUploads.Id.Flag.Post.Route (FlagForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int64)
import Data.Text (Text)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEphemeralUpload, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, nonExistentId)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.EphemeralUploads.Id.Flag.Post.Handler" $ do
      describe "action" $ do
        it "invalid reason returns validation error" test_invalidReasonReturnsValidationError
        it "TargetNotFound for non-existent upload" test_targetNotFoundForNonExistentUpload
        it "flags existing upload" test_flagsExistingUpload

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

test_invalidReasonReturnsValidationError :: TestDBConfig -> IO ()
test_invalidReasonReturnsValidationError cfg = do
  userInsert <- mkUserInsert "eu-flag-invalid" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
    userModel <- liftIO $ expectSetupRight dbResult

    let invalidForm = FlagForm {ffReason = "invalid-reason"}
    result <- runExceptT $ action userModel (EphemeralUploads.Id nonExistentId) invalidForm

    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"

test_targetNotFoundForNonExistentUpload :: TestDBConfig -> IO ()
test_targetNotFoundForNonExistentUpload cfg = do
  userInsert <- mkUserInsert "eu-flag-notfound" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
    userModel <- liftIO $ expectSetupRight dbResult

    let validForm = FlagForm {ffReason = "Inappropriate content"}
    result <- runExceptT $ action userModel (EphemeralUploads.Id nonExistentId) validForm

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right with TargetNotFound but got Left: " <> show err
      Right vd -> case ufvResult vd of
        TargetNotFound _ -> pure ()
        FlagSuccess _ -> expectationFailure "Expected TargetNotFound but got FlagSuccess"
        FlagFailed _ -> expectationFailure "Expected TargetNotFound but got FlagFailed"

test_flagsExistingUpload :: TestDBConfig -> IO ()
test_flagsExistingUpload cfg = do
  userInsert <- mkUserInsert "eu-flag-success" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      userModel <- TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
      uploadId <- insertTestEphemeralUpload (mkEphemeralUploadInsert "Flag Test Upload" userId)
      pure (userModel, uploadId)
    (userModel, uploadId) <- liftIO $ expectSetupRight dbResult

    let validForm = FlagForm {ffReason = "Inappropriate content"}
    result <- runExceptT $ action userModel uploadId validForm

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right with FlagSuccess but got Left: " <> show err
      Right vd -> case ufvResult vd of
        FlagSuccess _ -> pure ()
        TargetNotFound _ -> expectationFailure "Expected FlagSuccess but got TargetNotFound"
        FlagFailed _ -> expectationFailure "Expected FlagSuccess but got FlagFailed"
