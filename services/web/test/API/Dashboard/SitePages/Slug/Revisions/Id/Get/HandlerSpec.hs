{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.SitePages.Slug.Revisions.Id.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Slug.Revisions.Id.Get.Handler (PageRevisionViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.SitePageRevisions qualified as SitePageRevisions
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, nonExistentId, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.SitePages.Slug.Revisions.Id.Get.Handler" $ do
      describe "action" $ do
        it "NotFound for non-existent page slug" test_notFoundForMissingSlug
        it "NotFound for non-existent revision" test_notFoundForMissingRevision
        it "returns revision detail" test_returnsRevisionDetail

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Non-existent page slug returns NotFound.
test_notFoundForMissingSlug :: TestDBConfig -> IO ()
test_notFoundForMissingSlug cfg = do
  userInsert <- mkUserInsert "sp-rev-get-staff" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult
    result <- runExceptT $ action userModel userMetaModel "nonexistent-slug" (SitePageRevisions.Id nonExistentId)
    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Valid page slug with non-existent revision ID returns NotFound.
--
-- NOTE: Depends on the "about" page being pre-seeded by database migrations.
test_notFoundForMissingRevision :: TestDBConfig -> IO ()
test_notFoundForMissingRevision cfg = do
  userInsert <- mkUserInsert "sp-rev-get-admin" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult
    result <- runExceptT $ action userModel userMetaModel "about" (SitePageRevisions.Id nonExistentId)
    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Fetching a revision that was just inserted returns the correct content.
--
-- NOTE: Depends on the "about" page being pre-seeded by database migrations.
test_returnsRevisionDetail :: TestDBConfig -> IO ()
test_returnsRevisionDetail cfg = do
  userInsert <- mkUserInsert "sp-rev-get-detail" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      mPage <- TRX.statement () (SitePages.getPageBySlug "about")
      page <- maybe (error "about page not found") pure mPage
      let revInsert =
            SitePageRevisions.Insert
              { SitePageRevisions.spriPageId = SitePages.spmId page,
                SitePageRevisions.spriContent = "Revision content for detail test",
                SitePageRevisions.spriEditSummary = Just "Detail test revision",
                SitePageRevisions.spriCreatedBy = userModel.mId
              }
      revId <- unwrapInsert (SitePageRevisions.insertRevision revInsert)
      pure (userModel, userMetaModel, revId)
    (userModel, userMetaModel, revId) <- liftIO $ expectSetupRight dbResult
    result <- runExceptT $ action userModel userMetaModel "about" revId
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd ->
        SitePageRevisions.sprContent (prvRevision vd) `shouldBe` "Revision content for detail test"
