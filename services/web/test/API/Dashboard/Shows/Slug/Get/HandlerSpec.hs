{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}

module API.Dashboard.Shows.Slug.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Get.Handler (ShowDetailViewData (..), action)
import App.Handler.Error (HandlerError (..))
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
import "kpbj-web" App.Monad (AppM)

--------------------------------------------------------------------------------

-- | The handler checks access to the show with
-- 'App.Handler.Combinators.authorizeShow' before 'action' runs.
-- "App.Auth.GateSpec" tests that check.
spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Shows.Slug.Get.Handler" $ do
      describe "action" $ do
        it "returns the show when the show ID names it" test_returnsShow
        it "returns NotFound when the show ID names another show" test_notFoundForOtherShowId

--------------------------------------------------------------------------------

-- | Two active shows and a Staff user who hosts neither.
setupTwoShows ::
  TestDBConfig ->
  (UserMetadata.Model -> Shows.Model -> Shows.Model -> AppM ()) ->
  IO ()
setupTwoShows cfg k = do
  userInsert <- mkUserInsert "slug-get-staff" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (_userModel, userMetaModel) <- setupUserModels userInsert
      let firstInsert = Shows.Insert "Slug Get First Show" "slug-get-first-show" Nothing Nothing Shows.Active
          secondInsert = Shows.Insert "Slug Get Second Show" "slug-get-second-show" Nothing Nothing Shows.Active
      (firstId, _) <- insertTestShowWithSchedule firstInsert defaultScheduleInsert
      (secondId, _) <- insertTestShowWithSchedule secondInsert defaultScheduleInsert
      firstShow <- TRX.statement () (Shows.getShowById firstId) >>= maybe (error "show not found") pure
      secondShow <- TRX.statement () (Shows.getShowById secondId) >>= maybe (error "show not found") pure
      pure (userMetaModel, firstShow, secondShow)
    (userMetaModel, firstShow, secondShow) <- expectSetupRight dbResult
    k userMetaModel firstShow secondShow

-- | The action returns the show of the slug when the show ID names it. The
-- user does not host the show, and the action does not check access.
test_returnsShow :: TestDBConfig -> IO ()
test_returnsShow cfg = setupTwoShows cfg $ \userMetaModel firstShow _ -> do
  result <- runExceptT $ action userMetaModel firstShow.id firstShow Nothing
  liftIO $ case result of
    Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
    Right vd -> Shows.id vd.sdvShowModel `shouldBe` Shows.id firstShow

-- | The ID of one show with the slug of another show is not found.
test_notFoundForOtherShowId :: TestDBConfig -> IO ()
test_notFoundForOtherShowId cfg = setupTwoShows cfg $ \userMetaModel firstShow secondShow -> do
  result <- runExceptT $ action userMetaModel secondShow.id firstShow Nothing
  liftIO $ case result of
    Left (NotFound _) -> pure ()
    Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
    Right _ -> expectationFailure "Expected Left NotFound but got Right"
