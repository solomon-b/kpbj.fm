module API.Dashboard.EphemeralUploads.Id.Unflag.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Id.Unflag.Post.Handler (UnflagResult (..), UploadUnflagViewData (..), action)
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
    describe "API.Dashboard.EphemeralUploads.Id.Unflag.Post.Handler" $ do
      describe "action" $ do
        it "TargetNotFound for non-existent upload" test_targetNotFoundForNonExistentUpload
        it "successfully unflags a flagged upload" test_successfullyUnflagsFlaggedUpload

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

test_targetNotFoundForNonExistentUpload :: TestDBConfig -> IO ()
test_targetNotFoundForNonExistentUpload cfg = do
  bracketAppM cfg $ do
    result <- runExceptT $ action (EphemeralUploads.Id nonExistentId)

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right with TargetNotFound but got Left: " <> show err
      Right vd -> case uuvResult vd of
        TargetNotFound _ -> pure ()
        UnflagSuccess _ -> expectationFailure "Expected TargetNotFound but got UnflagSuccess"
        UnflagFailed _ -> expectationFailure "Expected TargetNotFound but got UnflagFailed"

test_successfullyUnflagsFlaggedUpload :: TestDBConfig -> IO ()
test_successfullyUnflagsFlaggedUpload cfg = do
  userInsert <- mkUserInsert "eu-unflag-success" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      uploadId <- insertTestEphemeralUpload (mkEphemeralUploadInsert "Unflag Test Upload" userId)
      _ <- TRX.statement () (EphemeralUploads.flagEphemeralUpload uploadId userId EphemeralUploads.InappropriateContent)
      pure uploadId
    uploadId <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action uploadId

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right with UnflagSuccess but got Left: " <> show err
      Right vd -> case uuvResult vd of
        UnflagSuccess _ -> pure ()
        TargetNotFound _ -> expectationFailure "Expected UnflagSuccess but got TargetNotFound"
        UnflagFailed _ -> expectationFailure "Expected UnflagSuccess but got UnflagFailed"
