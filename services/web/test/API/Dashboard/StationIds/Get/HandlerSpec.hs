module API.Dashboard.StationIds.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.StationIds.Get.Handler (StationIdsViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int64)
import Data.Text (Text)
import Domain.Types.PageNumber (PageNumber (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestStationId)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.StationIds.Get.Handler" $ do
      describe "action" $ do
        it "host user on empty DB gets empty station IDs list" test_emptyListOnFreshDB
        it "returns inserted station ID in list" test_returnsStationIdOnInsert
        it "pagination defaults to page 1" test_paginationDefaultsToPageOne

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

test_emptyListOnFreshDB :: TestDBConfig -> IO ()
test_emptyListOnFreshDB cfg = do
  userInsert <- mkUserInsert "si-get-empty" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> sivStationIds vd `shouldBe` []

test_returnsStationIdOnInsert :: TestDBConfig -> IO ()
test_returnsStationIdOnInsert cfg = do
  userInsert <- mkUserInsert "si-get-insert" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      _ <- insertTestStationId (mkStationIdInsert "Get Test Station ID" userModel.mId)
      pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> length (sivStationIds vd) `shouldBe` 1

test_paginationDefaultsToPageOne :: TestDBConfig -> IO ()
test_paginationDefaultsToPageOne cfg = do
  userInsert <- mkUserInsert "si-get-paginate" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> sivPage vd `shouldBe` PageNumber 1
