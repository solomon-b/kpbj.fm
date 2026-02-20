module API.Dashboard.SitePages.Slug.Revisions.Id.Restore.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Slug.Revisions.Id.Restore.Post.Handler (action)
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
import Test.Database.Helpers (insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, nonExistentId)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.SitePages.Slug.Revisions.Id.Restore.Post.Handler" $ do
      describe "action" $ do
        it "NotFound for non-existent page slug" test_notFoundForMissingSlug
        it "restores revision content" test_restoresRevisionContent

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Non-existent page slug returns NotFound.
test_notFoundForMissingSlug :: TestDBConfig -> IO ()
test_notFoundForMissingSlug cfg = do
  userInsert <- mkUserInsert "sp-restore-staff" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
    userModel <- liftIO $ expectSetupRight dbResult
    result <- runExceptT $ action userModel "nonexistent-slug" (SitePageRevisions.Id nonExistentId)
    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Restoring a valid revision for the "about" page succeeds (returns Right).
--
-- NOTE: Depends on the "about" page being pre-seeded by database migrations.
test_restoresRevisionContent :: TestDBConfig -> IO ()
test_restoresRevisionContent cfg = do
  userInsert <- mkUserInsert "sp-restore-admin" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      userModel <- TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
      mPage <- TRX.statement () (SitePages.getPageBySlug "about")
      page <- maybe (error "about page not found") pure mPage
      let revInsert =
            SitePageRevisions.Insert
              { SitePageRevisions.spriPageId = SitePages.spmId page,
                SitePageRevisions.spriContent = "Content to restore from",
                SitePageRevisions.spriEditSummary = Just "Restore test revision",
                SitePageRevisions.spriCreatedBy = userId
              }
      revId <- unwrapInsert (SitePageRevisions.insertRevision revInsert)
      pure (userModel, revId)
    (userModel, revId) <- liftIO $ expectSetupRight dbResult
    result <- runExceptT $ action userModel "about" revId
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()
