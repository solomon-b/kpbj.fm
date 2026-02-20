module API.Dashboard.StationBlog.New.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.New.Get.Handler (StationBlogNewViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestShowWithSchedule)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.StationBlog.New.Get.Handler" $ do
      describe "action" $ do
        it "staff user gets valid view data with metadata" test_returnsViewData
        it "inserted show appears in snvdAllShows for admin user" test_showsAppearInSidebar

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Staff user calling action receives view data with matching metadata.
test_returnsViewData :: TestDBConfig -> IO ()
test_returnsViewData cfg = do
  userInsert <- mkUserInsert "sb-new-get-vd" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd ->
        UserMetadata.mId (snvdUserMetadata vd) `shouldBe` UserMetadata.mId userMetaModel

-- | Admin user sees inserted show in snvdAllShows sidebar.
test_showsAppearInSidebar :: TestDBConfig -> IO ()
test_showsAppearInSidebar cfg = do
  userInsert <- mkUserInsert "sb-new-get-shows" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      let showInsert = Shows.Insert "Station Blog Sidebar Show" (Slug "sb-new-get-sidebar-show") Nothing Nothing Shows.Active
      (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      showModel <- TRX.statement () (Shows.getShowById showId) >>= maybe (error "show not found") pure
      pure (userModel, userMetaModel, showModel)
    (userModel, userMetaModel, showModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        let matchingShows = filter (\s -> Shows.id s == Shows.id showModel) (snvdAllShows vd)
        length matchingShows `shouldBe` 1
