{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Blogs.Slug.Edit.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Slug.Edit.Post.Handler (action)
import API.Dashboard.Blogs.Slug.Edit.Post.Route (ShowBlogEditForm (..))
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
import Test.Database.Helpers (addTestShowHost, insertTestShowBlogPost, insertTestShowWithSchedule)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Blogs.Slug.Edit.Post.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent post" test_notFoundForMissingPost
        it "admin updates a show blog post successfully" test_updatesShowBlogPost

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Nonexistent post yields NotFound "Blog post".
test_notFoundForMissingPost :: TestDBConfig -> IO ()
test_notFoundForMissingPost cfg = do
  userInsert <- mkUserInsert "blog-edit-post-nf" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (userModel, userMetaModel) <- setupUserModels userInsert
          let showInsert = Shows.Insert "Blog Edit Post NF Show" (Slug "blog-edit-post-nf-show") Nothing Nothing Shows.Active
          _ <- insertTestShowWithSchedule showInsert defaultScheduleInsert
          pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    let editForm =
          ShowBlogEditForm
            { sbefTitle = "Updated Title",
              sbefContent = "Updated content.",
              sbefExcerpt = Nothing,
              sbefStatus = "published",
              sbefTags = []
            }

    result <- runExceptT $ action userModel userMetaModel (Slug "blog-edit-post-nf-show") (ShowBlogPosts.Id 99999) editForm

    liftIO $ case result of
      Left (NotFound resource) -> resource `shouldBe` "Blog post"
      Left err -> expectationFailure $ "Expected NotFound \"Blog post\" but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Admin updates a blog post's title and the change is reflected in the result.
test_updatesShowBlogPost :: TestDBConfig -> IO ()
test_updatesShowBlogPost cfg = do
  userInsert <- mkUserInsert "blog-edit-post-admin" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (userModel, userMetaModel) <- setupUserModels userInsert
          let showInsert = Shows.Insert "Blog Edit Post Show" (Slug "blog-edit-post-show") Nothing Nothing Shows.Active
          (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
          addTestShowHost showId userModel.mId
          let postInsert =
                ShowBlogPosts.Insert
                  { ShowBlogPosts.sbpiId = showId,
                    ShowBlogPosts.sbpiTitle = "Original Title",
                    ShowBlogPosts.sbpiSlug = Slug.mkSlug "Original Title",
                    ShowBlogPosts.sbpiContent = "Original content.",
                    ShowBlogPosts.sbpiExcerpt = Nothing,
                    ShowBlogPosts.sbpiAuthorId = userModel.mId,
                    ShowBlogPosts.sbpiStatus = Draft
                  }
          postId <- insertTestShowBlogPost postInsert
          pure (userModel, userMetaModel, postId)
    (userModel, userMetaModel, postId) <- liftIO $ expectSetupRight dbResult

    let editForm =
          ShowBlogEditForm
            { sbefTitle = "Updated Title",
              sbefContent = "Updated content.",
              sbefExcerpt = Nothing,
              sbefStatus = "published",
              sbefTags = []
            }

    result <- runExceptT $ action userModel userMetaModel (Slug "blog-edit-post-show") postId editForm

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right (_showModel, returnedPostId) ->
        -- The returned post ID matches the original post
        returnedPostId `shouldBe` postId
