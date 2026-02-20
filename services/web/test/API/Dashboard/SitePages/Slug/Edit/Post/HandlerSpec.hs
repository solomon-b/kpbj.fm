module API.Dashboard.SitePages.Slug.Edit.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Slug.Edit.Post.Handler (action)
import API.Dashboard.SitePages.Slug.Edit.Post.Route (EditForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.SitePages.Slug.Edit.Post.Handler" $ do
      describe "action" $ do
        it "NotFound for non-existent slug" test_notFoundForMissingSlug
        it "updates existing page content" test_updatesExistingPage
        it "no changes returns Right" test_noChangesReturnsRight

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Non-existent slug returns NotFound.
test_notFoundForMissingSlug :: TestDBConfig -> IO ()
test_notFoundForMissingSlug cfg = do
  userInsert <- mkUserInsert "sp-edit-post-staff" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
    userModel <- liftIO $ expectSetupRight dbResult
    let editForm =
          EditForm
            { efTitle = "Some Title",
              efContent = "Some content.",
              efEditSummary = Nothing
            }
    result <- runExceptT $ action userModel "nonexistent-slug-12345" editForm
    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Passing updated title/content for the "about" page returns Right.
--
-- NOTE: Depends on the "about" page being pre-seeded by database migrations.
test_updatesExistingPage :: TestDBConfig -> IO ()
test_updatesExistingPage cfg = do
  userInsert <- mkUserInsert "sp-edit-post-admin" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
    userModel <- liftIO $ expectSetupRight dbResult
    let editForm =
          EditForm
            { efTitle = "About KPBJ Updated",
              efContent = "Updated about page content for testing.",
              efEditSummary = Just "Test update"
            }
    result <- runExceptT $ action userModel "about" editForm
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

-- | Submitting identical content to the current page returns Right (no error).
--
-- NOTE: Depends on the "about" page being pre-seeded by database migrations.
test_noChangesReturnsRight :: TestDBConfig -> IO ()
test_noChangesReturnsRight cfg = do
  userInsert <- mkUserInsert "sp-edit-post-nochange" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      userModel <- TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
      mPage <- TRX.statement () (SitePages.getPageBySlug "about")
      page <- maybe (error "about page not found") pure mPage
      pure (userModel, page)
    (userModel, page) <- liftIO $ expectSetupRight dbResult
    let editForm =
          EditForm
            { efTitle = SitePages.spmTitle page,
              efContent = SitePages.spmContent page,
              efEditSummary = Nothing
            }
    result <- runExceptT $ action userModel "about" editForm
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()
