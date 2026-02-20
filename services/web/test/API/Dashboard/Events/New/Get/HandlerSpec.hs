{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.New.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Events.New.Get.Handler (NewEventViewData (..), action)
import App.Handler.Error ()
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
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
    describe "API.Dashboard.Events.New.Get.Handler" $ do
      describe "action" $ do
        it "returns view data with user metadata for staff user" test_returnsViewData
        it "shows appear in nevAllShows for admin user" test_showsAppearInSidebar

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Staff user gets valid view data containing their user metadata.
test_returnsViewData :: TestDBConfig -> IO ()
test_returnsViewData cfg = do
  userInsert <- mkUserInsert "ev-new-get-staff" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> vd.nevUserMetadata `shouldBe` userMetaModel

-- | For an admin user, shows inserted into the DB appear in nevAllShows.
test_showsAppearInSidebar :: TestDBConfig -> IO ()
test_showsAppearInSidebar cfg = do
  userInsert <- mkUserInsert "ev-new-get-admin" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      let showInsert =
            Shows.Insert
              { Shows.siTitle = "Event Sidebar Show",
                Shows.siSlug = "ev-new-get-sidebar-show",
                Shows.siDescription = Nothing,
                Shows.siLogoUrl = Nothing,
                Shows.siStatus = Shows.Active
              }
      (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      showModel <- TRX.statement () (Shows.getShowById showId) >>= maybe (error "show not found") pure
      pure (userModel, userMetaModel, showModel)
    (userModel, userMetaModel, showModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        let showIds = map Shows.id vd.nevAllShows
        elem showModel.id showIds `shouldBe` True
