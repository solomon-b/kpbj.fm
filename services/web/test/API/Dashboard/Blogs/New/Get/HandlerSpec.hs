module API.Dashboard.Blogs.New.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.New.Get.Handler (BlogNewViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
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
    describe "API.Dashboard.Blogs.New.Get.Handler" $ do
      describe "action" $ do
        it "admin can access new blog post form for any show" test_adminCanAccessNewBlogForm
        it "non-host user gets NotAuthorized" test_nonHostGetsNotAuthorized

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Admin user can access the new blog post form for any show.
test_adminCanAccessNewBlogForm :: TestDBConfig -> IO ()
test_adminCanAccessNewBlogForm cfg = do
  userInsert <- mkUserInsert "blog-new-get-admin" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (userModel, userMetaModel) <- setupUserModels userInsert
          let showInsert = Shows.Insert "Blog New Get Show" (Slug "blog-new-get-show") Nothing Nothing Shows.Active
          _ <- insertTestShowWithSchedule showInsert defaultScheduleInsert
          pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (Slug "blog-new-get-show")

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> bnvShowModel vd `shouldBe` bnvShowModel vd -- show was fetched

-- | Regular User role is not a host of the show and gets NotAuthorized.
test_nonHostGetsNotAuthorized :: TestDBConfig -> IO ()
test_nonHostGetsNotAuthorized cfg = do
  userInsert <- mkUserInsert "blog-new-get-user" UserMetadata.User

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (userModel, userMetaModel) <- setupUserModels userInsert
          let showInsert = Shows.Insert "Blog New Get Unrelated Show" (Slug "blog-new-get-unrelated-show") Nothing Nothing Shows.Active
          _ <- insertTestShowWithSchedule showInsert defaultScheduleInsert
          pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (Slug "blog-new-get-unrelated-show")

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotAuthorized but got Right"
