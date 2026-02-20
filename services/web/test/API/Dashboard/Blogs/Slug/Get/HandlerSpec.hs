{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Blogs.Slug.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Slug.Get.Handler (BlogDetailViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug (..))
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (addTestShowHost, insertTestShowBlogPost, insertTestShowWithSchedule, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Blogs.Slug.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent show slug" test_notFoundForMissingShow
        it "admin can view show blog post detail" test_adminCanViewPostDetail
        it "unrelated user not host of show gets NotAuthorized" test_notAuthorizedForUnrelatedUser

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Nonexistent show slug yields NotFound "Show".
test_notFoundForMissingShow :: TestDBConfig -> IO ()
test_notFoundForMissingShow cfg = do
  userInsert <- mkUserInsert "blog-slug-get-nf" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (Slug "blog-slug-get-nonexistent-show") (ShowBlogPosts.Id 99999)

    liftIO $ case result of
      Left (NotFound resource) -> resource `shouldBe` "Show"
      Left err -> expectationFailure $ "Expected NotFound \"Show\" but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Admin can view blog post detail for any show.
test_adminCanViewPostDetail :: TestDBConfig -> IO ()
test_adminCanViewPostDetail cfg = do
  userInsert <- mkUserInsert "blog-slug-get-admin" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (userModel, userMetaModel) <- setupUserModels userInsert
          let showInsert = Shows.Insert "Blog Slug Get Show" (Slug "blog-slug-get-show") Nothing Nothing Shows.Active
          (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
          let postInsert =
                ShowBlogPosts.Insert
                  { ShowBlogPosts.sbpiId = showId,
                    ShowBlogPosts.sbpiTitle = "Detail Post",
                    ShowBlogPosts.sbpiSlug = Slug.mkSlug "Detail Post",
                    ShowBlogPosts.sbpiContent = "Content here.",
                    ShowBlogPosts.sbpiExcerpt = Nothing,
                    ShowBlogPosts.sbpiAuthorId = userModel.mId,
                    ShowBlogPosts.sbpiStatus = Published
                  }
          postId <- insertTestShowBlogPost postInsert
          pure (userModel, userMetaModel, postId)
    (userModel, userMetaModel, postId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (Slug "blog-slug-get-show") postId

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> bdvBlogPost vd `shouldBe` bdvBlogPost vd -- post was fetched

-- | Regular user who is not a host of the show gets NotAuthorized.
test_notAuthorizedForUnrelatedUser :: TestDBConfig -> IO ()
test_notAuthorizedForUnrelatedUser cfg = do
  adminInsert <- mkUserInsert "blog-slug-get-owner" UserMetadata.Admin
  userInsert <- mkUserInsert "blog-slug-get-unauth" UserMetadata.User

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          adminId <- insertTestUser adminInsert
          (userModel, userMetaModel) <- setupUserModels userInsert
          let showInsert = Shows.Insert "Blog Slug Get Auth Show" (Slug "blog-slug-get-auth-show") Nothing Nothing Shows.Active
          (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
          addTestShowHost showId adminId
          let postInsert =
                ShowBlogPosts.Insert
                  { ShowBlogPosts.sbpiId = showId,
                    ShowBlogPosts.sbpiTitle = "Auth Check Post",
                    ShowBlogPosts.sbpiSlug = Slug.mkSlug "Auth Check Post",
                    ShowBlogPosts.sbpiContent = "Content.",
                    ShowBlogPosts.sbpiExcerpt = Nothing,
                    ShowBlogPosts.sbpiAuthorId = adminId,
                    ShowBlogPosts.sbpiStatus = Published
                  }
          postId <- insertTestShowBlogPost postInsert
          pure (userModel, userMetaModel, postId)
    (userModel, userMetaModel, postId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (Slug "blog-slug-get-auth-show") postId

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotAuthorized but got Right"
