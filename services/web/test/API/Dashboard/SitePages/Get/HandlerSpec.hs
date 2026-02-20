module API.Dashboard.SitePages.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.SitePages.Get.Handler (PageListViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.SitePages.Get.Handler" $ do
      describe "action" $ do
        it "returns pre-seeded pages" test_returnsPreSeededPages
        it "returns Right for admin user" test_returnsRightForAdmin

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Staff user can list pages; pre-seeded pages are returned.
test_returnsPreSeededPages :: TestDBConfig -> IO ()
test_returnsPreSeededPages cfg = do
  userInsert <- mkUserInsert "sp-list-staff" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult
    result <- runExceptT $ action userModel userMetaModel
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> length (plvPages vd) `shouldSatisfy` (> 0)

-- | Admin user also gets Right result.
test_returnsRightForAdmin :: TestDBConfig -> IO ()
test_returnsRightForAdmin cfg = do
  userInsert <- mkUserInsert "sp-list-admin" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult
    result <- runExceptT $ action userModel userMetaModel
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()
