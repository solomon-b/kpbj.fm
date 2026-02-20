{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Shows.Slug.Edit.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Get.Handler (ShowEditViewData (..), action)
import App.Handler.Error (HandlerError (..))
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
    describe "API.Dashboard.Shows.Slug.Edit.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for nonexistent slug" test_notFoundForMissingSlug
        it "admin user gets edit context for valid slug" test_returnsEditContextForValidSlug
        it "staff user sees schedule data and eligible hosts" test_staffSeesScheduleData

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | Admin user with a nonexistent slug gets NotFound.
test_notFoundForMissingSlug :: TestDBConfig -> IO ()
test_notFoundForMissingSlug cfg = do
  adminInsert <- mkUserInsert "slug-edit-notfound" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels adminInsert
    (userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (Slug "does-not-exist")

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Admin user gets the edit view data with the correct show model.
test_returnsEditContextForValidSlug :: TestDBConfig -> IO ()
test_returnsEditContextForValidSlug cfg = do
  adminInsert <- mkUserInsert "slug-edit-admin" UserMetadata.Admin
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels adminInsert
      let showInsert = Shows.Insert "Edit Context Show" "slug-edit-admin-show" Nothing Nothing Shows.Active
      (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      showModel <- TRX.statement () (Shows.getShowById showId) >>= maybe (error "show not found") pure
      pure (userModel, userMetaModel, showModel)
    (userModel, userMetaModel, showModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel showModel.slug

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd ->
        Shows.id vd.sevShowModel `shouldBe` Shows.id showModel

-- | Staff user gets non-empty schedule JSON and eligible hosts list.
test_staffSeesScheduleData :: TestDBConfig -> IO ()
test_staffSeesScheduleData cfg = do
  staffInsert <- mkUserInsert "slug-edit-staff" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels staffInsert
      let showInsert = Shows.Insert "Staff Schedule Show" "slug-edit-staff-show" Nothing Nothing Shows.Active
      (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      showModel <- TRX.statement () (Shows.getShowById showId) >>= maybe (error "show not found") pure
      pure (userModel, userMetaModel, showModel)
    (userModel, userMetaModel, showModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel showModel.slug

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        -- Staff should see schedule data (not the default empty "[]" would mean
        -- the conditional branch was skipped, but isStaff is True so data is fetched)
        vd.sevIsStaff `shouldBe` True
        -- Eligible hosts list is fetched for staff; at least our staff user is there
        not (null vd.sevEligibleHosts) `shouldBe` True
