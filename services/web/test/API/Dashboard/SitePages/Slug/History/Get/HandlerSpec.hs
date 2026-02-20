{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.SitePages.Slug.History.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Slug.History.Get.Handler (PageHistoryViewData (..), action)
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
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.SitePages.Slug.History.Get.Handler" $ do
      describe "action" $ do
        it "NotFound for non-existent slug" test_notFoundForMissingSlug
        it "returns history for about page" test_returnsHistoryForAbout
        it "includes inserted revision" test_includesInsertedRevision

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Non-existent slug returns NotFound.
test_notFoundForMissingSlug :: TestDBConfig -> IO ()
test_notFoundForMissingSlug cfg = do
  userInsert <- mkUserInsert "sp-hist-staff" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult
    result <- runExceptT $ action userModel userMetaModel "nonexistent-slug-12345"
    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Pre-seeded "about" page history is returned with the correct page slug.
--
-- NOTE: Depends on the "about" page being pre-seeded by database migrations.
test_returnsHistoryForAbout :: TestDBConfig -> IO ()
test_returnsHistoryForAbout cfg = do
  userInsert <- mkUserInsert "sp-hist-admin" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult
    result <- runExceptT $ action userModel userMetaModel "about"
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> SitePages.spmSlug (phvPage vd) `shouldBe` "about"

-- | A manually inserted revision shows up in the history list.
--
-- NOTE: Depends on the "about" page being pre-seeded by database migrations.
test_includesInsertedRevision :: TestDBConfig -> IO ()
test_includesInsertedRevision cfg = do
  userInsert <- mkUserInsert "sp-hist-revision" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      mPage <- TRX.statement () (SitePages.getPageBySlug "about")
      page <- maybe (error "about page not found") pure mPage
      let revInsert =
            SitePageRevisions.Insert
              { SitePageRevisions.spriPageId = SitePages.spmId page,
                SitePageRevisions.spriContent = "Previous content snapshot",
                SitePageRevisions.spriEditSummary = Just "Test revision",
                SitePageRevisions.spriCreatedBy = userModel.mId
              }
      _ <- unwrapInsert (SitePageRevisions.insertRevision revInsert)
      pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult
    result <- runExceptT $ action userModel userMetaModel "about"
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> length (phvRevisions vd) `shouldSatisfy` (> 0)
