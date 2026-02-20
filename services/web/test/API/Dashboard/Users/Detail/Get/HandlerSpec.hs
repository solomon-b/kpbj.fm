{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Users.Detail.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Detail.Get.Handler (UserDetailViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, nonExistentId, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Users.Detail.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for missing user" test_notFoundForMissingUser
        it "returns detail for existing user" test_returnsDetailForExistingUser
        it "returns hosted shows and recent episodes" test_returnsActivityData

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Returns NotFound when the target user does not exist in the DB.
test_notFoundForMissingUser :: TestDBConfig -> IO ()
test_notFoundForMissingUser cfg = do
  viewerInsert <- mkUserInsert "usr-detail-viewer-notfound" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (viewerModel, viewerMeta) <- setupUserModels viewerInsert
      pure (viewerModel, viewerMeta)
    (viewerModel, viewerMeta) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action viewerModel viewerMeta (User.Id nonExistentId)

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Returns full detail view data for an existing user.
test_returnsDetailForExistingUser :: TestDBConfig -> IO ()
test_returnsDetailForExistingUser cfg = do
  viewerInsert <- mkUserInsert "usr-detail-viewer-exists" UserMetadata.Staff
  targetInsert <- mkUserInsert "usr-detail-target-exists" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (viewerModel, viewerMeta) <- setupUserModels viewerInsert
      targetId <- insertTestUser targetInsert
      pure (viewerModel, viewerMeta, targetId)
    (viewerModel, viewerMeta, targetId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action viewerModel viewerMeta targetId

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> vd.udvTargetUser.mId `shouldBe` targetId

-- | Activity data (hosted shows, recent episodes) is returned (empty lists are fine).
test_returnsActivityData :: TestDBConfig -> IO ()
test_returnsActivityData cfg = do
  viewerInsert <- mkUserInsert "usr-detail-viewer-activity" UserMetadata.Admin
  targetInsert <- mkUserInsert "usr-detail-target-activity" UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (viewerModel, viewerMeta) <- setupUserModels viewerInsert
      targetId <- insertTestUser targetInsert
      pure (viewerModel, viewerMeta, targetId)
    (viewerModel, viewerMeta, targetId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action viewerModel viewerMeta targetId

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        -- No shows or episodes set up, so both should be empty
        vd.udvHostedShows `shouldBe` []
        vd.udvRecentEpisodes `shouldBe` []
