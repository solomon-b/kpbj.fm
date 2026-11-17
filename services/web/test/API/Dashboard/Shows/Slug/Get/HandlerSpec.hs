{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Shows.Slug.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Get.Handler (ShowDetailViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (addTestShowHost, insertTestShowWithSchedule)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert, nonExistentId, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Shows.Slug.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for nonexistent show ID" test_notFoundForMissingShow
        it "admin user can view show by ID" test_returnsShowDetailForAdmin
        it "non-host regular user cannot view show" test_notFoundForNonHostUser
        it "host of the show can view it" test_hostOfShowCanView
        it "host who does not host the show gets NotFound" test_notFoundForUnrelatedHost
        it "staff user who does not host the show gets NotFound" test_notFoundForNonHostStaff

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Admin user with a nonexistent show ID gets NotFound.
test_notFoundForMissingShow :: TestDBConfig -> IO ()
test_notFoundForMissingShow cfg = do
  adminInsert <- mkUserInsert "slug-get-notfound" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels adminInsert
      pure (userModel, userMetaModel)
    (_userModel, userMetaModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel (Shows.Id nonExistentId) (Slug "nonexistent-show") Nothing

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Admin user can view a show by ID; the returned model matches.
test_returnsShowDetailForAdmin :: TestDBConfig -> IO ()
test_returnsShowDetailForAdmin cfg = do
  adminInsert <- mkUserInsert "slug-get-admin" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels adminInsert
      let showInsert = Shows.Insert "Admin Show Detail" "slug-get-admin-show" Nothing Nothing Shows.Active
      (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      showModel <- TRX.statement () (Shows.getShowById showId) >>= maybe (error "show not found") pure
      pure (userModel, userMetaModel, showModel)
    (_userModel, userMetaModel, showModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel showModel.id showModel.slug Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd ->
        Shows.id vd.sdvShowModel `shouldBe` Shows.id showModel

-- | A regular user (not a host) cannot see a show — action returns NotFound.
--
-- fetchDashboardShows only returns shows where the user is an active host.
-- A non-host, non-admin user will see an empty sidebar and the find will fail.
test_notFoundForNonHostUser :: TestDBConfig -> IO ()
test_notFoundForNonHostUser cfg = do
  userInsert <- mkUserInsert "slug-get-nonhost" UserMetadata.User

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      let showInsert = Shows.Insert "Non-Host Show" "slug-get-nonhost-show" Nothing Nothing Shows.Active
      (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      showModel <- TRX.statement () (Shows.getShowById showId) >>= maybe (error "show not found") pure
      pure (userModel, userMetaModel, showModel)
    (_userModel, userMetaModel, showModel) <- expectSetupRight dbResult

    -- The user is not a host of this show; they should not see it
    result <- runExceptT $ action userMetaModel showModel.id showModel.slug Nothing

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | A host can view a show that they host.
test_hostOfShowCanView :: TestDBConfig -> IO ()
test_hostOfShowCanView cfg = do
  userInsert <- mkUserInsert "slug-get-own-host" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      let showInsert = Shows.Insert "Own Host Show" "slug-get-own-host-show" Nothing Nothing Shows.Active
      (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      addTestShowHost showId userModel.mId
      showModel <- TRX.statement () (Shows.getShowById showId) >>= maybe (error "show not found") pure
      pure (userMetaModel, showModel)
    (userMetaModel, showModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel showModel.id showModel.slug Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd ->
        Shows.id vd.sdvShowModel `shouldBe` Shows.id showModel

-- | A host of one show gets NotFound for a show that they do not host.
test_notFoundForUnrelatedHost :: TestDBConfig -> IO ()
test_notFoundForUnrelatedHost cfg = do
  userInsert <- mkUserInsert "slug-get-other-host" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      let ownShowInsert = Shows.Insert "Other Host Own Show" "slug-get-other-host-own" Nothing Nothing Shows.Active
          otherShowInsert = Shows.Insert "Other Host Other Show" "slug-get-other-host-other" Nothing Nothing Shows.Active
      (ownShowId, _) <- insertTestShowWithSchedule ownShowInsert defaultScheduleInsert
      addTestShowHost ownShowId userModel.mId
      (otherShowId, _) <- insertTestShowWithSchedule otherShowInsert defaultScheduleInsert
      otherShowModel <- TRX.statement () (Shows.getShowById otherShowId) >>= maybe (error "show not found") pure
      pure (userMetaModel, otherShowModel)
    (userMetaModel, otherShowModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel otherShowModel.id otherShowModel.slug Nothing

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | A staff user who does not host the show gets NotFound.
--
-- Only an admin gets every active show. Staff get the shows that they host.
test_notFoundForNonHostStaff :: TestDBConfig -> IO ()
test_notFoundForNonHostStaff cfg = do
  userInsert <- mkUserInsert "slug-get-staff" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      let showInsert = Shows.Insert "Staff Non-Host Show" "slug-get-staff-show" Nothing Nothing Shows.Active
      (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      showModel <- TRX.statement () (Shows.getShowById showId) >>= maybe (error "show not found") pure
      pure (userModel, userMetaModel, showModel)
    (_userModel, userMetaModel, showModel) <- expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel showModel.id showModel.slug Nothing

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"
