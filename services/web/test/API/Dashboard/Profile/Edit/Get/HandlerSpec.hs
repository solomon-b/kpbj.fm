{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Profile.Edit.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Profile.Edit.Get.Handler (ProfileEditFormViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.StorageBackend (StorageBackend (..), defaultLocalConfig)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Profile.Edit.Get.Handler" $ do
      describe "action" $ do
        it "returns view data containing the correct user and metadata" test_returnsCorrectUserData
        it "returns LocalStorage backend in test environment" test_returnsLocalStorageBackend

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | The action returns a ProfileEditFormViewData whose pefUser and pefUserMetadata
-- match the user that was inserted for the test.
test_returnsCorrectUserData :: TestDBConfig -> IO ()
test_returnsCorrectUserData cfg = do
  userInsert <- mkUserInsert "profile-edit-get-correct" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (userModel, userMetaModel) <- setupUserModels userInsert
        pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult
    result <- runExceptT $ action userModel userMetaModel
    liftIO $ case result of
      Left err ->
        expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        User.mId vd.pefUser `shouldBe` User.mId userModel
        UserMetadata.mId vd.pefUserMetadata `shouldBe` UserMetadata.mId userMetaModel

-- | The test context always uses LocalStorage, so the action must surface a
-- LocalStorage backend in the view data.
test_returnsLocalStorageBackend :: TestDBConfig -> IO ()
test_returnsLocalStorageBackend cfg = do
  userInsert <- mkUserInsert "profile-edit-get-storage" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (userModel, userMetaModel) <- setupUserModels userInsert
        pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult
    result <- runExceptT $ action userModel userMetaModel
    liftIO $ case result of
      Left err ->
        expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd ->
        vd.pefStorageBackend `shouldBe` LocalStorage defaultLocalConfig
