{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Users.Edit.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Edit.Get.Handler (UserEditViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
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
    describe "API.Dashboard.Users.Edit.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for missing user" test_notFoundForMissingUser
        it "returns edit view data for existing user" test_returnsEditViewData
        it "target user and metadata match what was inserted" test_targetDataMatchesInsert

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Returns NotFound when the target user does not exist.
test_notFoundForMissingUser :: TestDBConfig -> IO ()
test_notFoundForMissingUser cfg = do
  viewerInsert <- mkUserInsert "usr-edit-get-viewer-notfound" UserMetadata.Staff
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

-- | Returns a view data record for an existing target user.
test_returnsEditViewData :: TestDBConfig -> IO ()
test_returnsEditViewData cfg = do
  viewerInsert <- mkUserInsert "usr-edit-get-viewer-exists" UserMetadata.Staff
  targetInsert <- mkUserInsert "usr-edit-get-target-exists" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (viewerModel, viewerMeta) <- setupUserModels viewerInsert
      targetId <- insertTestUser targetInsert
      pure (viewerModel, viewerMeta, targetId)
    (viewerModel, viewerMeta, targetId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action viewerModel viewerMeta targetId

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> vd.uevTargetUser.mId `shouldBe` targetId

-- | The returned target user and metadata IDs match what was inserted.
test_targetDataMatchesInsert :: TestDBConfig -> IO ()
test_targetDataMatchesInsert cfg = do
  viewerInsert <- mkUserInsert "usr-edit-get-viewer-match" UserMetadata.Admin
  targetInsert <- mkUserInsert "usr-edit-get-target-match" UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (viewerModel, viewerMeta) <- setupUserModels viewerInsert
      targetId <- insertTestUser targetInsert
      targetMeta <- TRX.statement () (UserMetadata.getUserMetadata targetId) >>= maybe (error "target metadata not found") pure
      pure (viewerModel, viewerMeta, targetId, targetMeta)
    (viewerModel, viewerMeta, targetId, targetMeta) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action viewerModel viewerMeta targetId

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        vd.uevTargetUser.mId `shouldBe` targetId
        vd.uevTargetMetadata.mId `shouldBe` targetMeta.mId
        vd.uevTargetMetadata.mUserRole `shouldBe` UserMetadata.Host
