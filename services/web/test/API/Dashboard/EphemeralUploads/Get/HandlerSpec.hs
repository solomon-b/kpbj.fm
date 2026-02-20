module API.Dashboard.EphemeralUploads.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.EphemeralUploads.Get.Handler (UploadListViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int64)
import Data.Text (Text)
import Domain.Types.PageNumber (PageNumber (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEphemeralUpload)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.EphemeralUploads.Get.Handler" $ do
      describe "action" $ do
        it "host user on empty DB gets empty uploads list" test_emptyListOnFreshDB
        it "returns inserted ephemeral upload in list" test_returnsUploadOnInsert
        it "pagination defaults to page 1" test_paginationDefaultsToPageOne

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

test_emptyListOnFreshDB :: TestDBConfig -> IO ()
test_emptyListOnFreshDB cfg = do
  userInsert <- mkUserInsert "eu-get-empty" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> ulvEphemeralUploads vd `shouldBe` []

test_returnsUploadOnInsert :: TestDBConfig -> IO ()
test_returnsUploadOnInsert cfg = do
  userInsert <- mkUserInsert "eu-get-insert" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      _ <- insertTestEphemeralUpload (mkEphemeralUploadInsert "Get Test Upload" userModel.mId)
      pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> length (ulvEphemeralUploads vd) `shouldBe` 1

test_paginationDefaultsToPageOne :: TestDBConfig -> IO ()
test_paginationDefaultsToPageOne cfg = do
  userInsert <- mkUserInsert "eu-get-paginate" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> ulvPage vd `shouldBe` PageNumber 1
