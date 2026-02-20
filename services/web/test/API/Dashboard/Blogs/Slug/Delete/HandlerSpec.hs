{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Blogs.Slug.Delete.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Slug.Delete.Handler (action)
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
import Hasql.Transaction qualified as TRX
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
    describe "API.Dashboard.Blogs.Slug.Delete.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent show slug" test_notFoundForMissingShow
        it "admin deletes a show blog post successfully" test_deletesShowBlogPost
        it "non-host user gets NotAuthorized" test_notAuthorizedForNonHost

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Nonexistent show slug yields NotFound "Show".
test_notFoundForMissingShow :: TestDBConfig -> IO ()
test_notFoundForMissingShow cfg = do
  userInsert <- mkUserInsert "blog-delete-nf" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (Slug "blog-delete-nonexistent-show") (ShowBlogPosts.Id 99999)

    liftIO $ case result of
      Left (NotFound resource) -> resource `shouldBe` "Show"
      Left err -> expectationFailure $ "Expected NotFound \"Show\" but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Admin deletes a blog post and it is no longer retrievable from the DB.
test_deletesShowBlogPost :: TestDBConfig -> IO ()
test_deletesShowBlogPost cfg = do
  userInsert <- mkUserInsert "blog-delete-admin" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (userModel, userMetaModel) <- setupUserModels userInsert
          let showInsert = Shows.Insert "Blog Delete Show" (Slug "blog-delete-show") Nothing Nothing Shows.Active
          (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
          addTestShowHost showId userModel.mId
          let postInsert =
                ShowBlogPosts.Insert
                  { ShowBlogPosts.sbpiId = showId,
                    ShowBlogPosts.sbpiTitle = "Post To Delete",
                    ShowBlogPosts.sbpiSlug = Slug.mkSlug "Post To Delete",
                    ShowBlogPosts.sbpiContent = "This will be deleted.",
                    ShowBlogPosts.sbpiExcerpt = Nothing,
                    ShowBlogPosts.sbpiAuthorId = userModel.mId,
                    ShowBlogPosts.sbpiStatus = Published
                  }
          postId <- insertTestShowBlogPost postInsert
          pure (userModel, userMetaModel, postId)
    (userModel, userMetaModel, postId) <- liftIO $ expectSetupRight dbResult

    -- Delete the post
    deleteResult <- runExceptT $ action userModel userMetaModel (Slug "blog-delete-show") postId

    liftIO $ case deleteResult of
      Left err -> expectationFailure $ "Expected delete to succeed but got: " <> show err
      Right () -> pure ()

    -- Verify the post is no longer in the DB
    fetchResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (ShowBlogPosts.getShowBlogPostById postId)

    liftIO $ case fetchResult of
      Left err -> expectationFailure $ "DB fetch after delete failed: " <> show err
      Right mPost -> mPost `shouldBe` Nothing

-- | Regular user who is not a host of the show gets NotAuthorized.
test_notAuthorizedForNonHost :: TestDBConfig -> IO ()
test_notAuthorizedForNonHost cfg = do
  hostInsert <- mkUserInsert "blog-delete-host" UserMetadata.Host
  nonHostInsert <- mkUserInsert "blog-delete-nonhost" UserMetadata.User

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          hostId <- insertTestUser hostInsert
          (nonHostModel, nonHostMetaModel) <- setupUserModels nonHostInsert
          let showInsert = Shows.Insert "Blog Delete Auth Show" (Slug "blog-delete-auth-show") Nothing Nothing Shows.Active
          (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
          addTestShowHost showId hostId
          let postInsert =
                ShowBlogPosts.Insert
                  { ShowBlogPosts.sbpiId = showId,
                    ShowBlogPosts.sbpiTitle = "Auth Test Post",
                    ShowBlogPosts.sbpiSlug = Slug.mkSlug "Auth Test Post",
                    ShowBlogPosts.sbpiContent = "Content.",
                    ShowBlogPosts.sbpiExcerpt = Nothing,
                    ShowBlogPosts.sbpiAuthorId = hostId,
                    ShowBlogPosts.sbpiStatus = Published
                  }
          postId <- insertTestShowBlogPost postInsert
          pure (nonHostModel, nonHostMetaModel, postId)
    (nonHostModel, nonHostMetaModel, postId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action nonHostModel nonHostMetaModel (Slug "blog-delete-auth-show") postId

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotAuthorized but got Right"
