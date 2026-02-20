module API.Dashboard.SitePages.Slug.Edit.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Slug.Edit.Get.Handler (PageEditViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.SitePages qualified as SitePages
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
    describe "API.Dashboard.SitePages.Slug.Edit.Get.Handler" $ do
      describe "action" $ do
        it "NotFound for non-existent slug" test_notFoundForMissingSlug
        it "returns edit data for about page" test_returnsEditDataForAbout

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Non-existent slug returns NotFound.
test_notFoundForMissingSlug :: TestDBConfig -> IO ()
test_notFoundForMissingSlug cfg = do
  userInsert <- mkUserInsert "sp-edit-get-staff" UserMetadata.Staff
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

-- | Pre-seeded "about" page is returned with correct slug.
test_returnsEditDataForAbout :: TestDBConfig -> IO ()
test_returnsEditDataForAbout cfg = do
  userInsert <- mkUserInsert "sp-edit-get-admin" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult
    result <- runExceptT $ action userModel userMetaModel "about"
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> SitePages.spmSlug (pevPage vd) `shouldBe` "about"
