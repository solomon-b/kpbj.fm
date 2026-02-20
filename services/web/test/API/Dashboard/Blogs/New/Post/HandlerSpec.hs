module API.Dashboard.Blogs.New.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.New.Post.Handler (action)
import API.Dashboard.Blogs.New.Post.Route (NewShowBlogPostForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (addTestShowHost, insertTestShowWithSchedule, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Blogs.New.Post.Handler" $ do
      describe "action" $ do
        it "admin creates a show blog post successfully" test_adminCreatesShowBlogPost
        it "non-host user gets NotAuthorized" test_nonHostGetsNotAuthorized

--------------------------------------------------------------------------------

-- | A minimal valid form for creating a new show blog post.
sampleForm :: NewShowBlogPostForm
sampleForm =
  NewShowBlogPostForm
    { nsbpfTitle = "My Test Post",
      nsbpfContent = "Post content goes here.",
      nsbpfExcerpt = Nothing,
      nsbpfStatus = Just "published",
      nsbpfTags = []
    }

--------------------------------------------------------------------------------

-- | Admin creates a blog post for a show and receives back a valid post ID.
test_adminCreatesShowBlogPost :: TestDBConfig -> IO ()
test_adminCreatesShowBlogPost cfg = do
  userInsert <- mkUserInsert "blog-new-post-admin" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (userModel, userMetaModel) <- setupUserModels userInsert
          let showInsert = Shows.Insert "Blog New Post Show" (Slug "blog-new-post-show") Nothing Nothing Shows.Active
          _ <- insertTestShowWithSchedule showInsert defaultScheduleInsert
          pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (Slug "blog-new-post-show") sampleForm

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right (postId, _showModel) -> do
        -- Verify the post ID is a positive integer (DB assigned)
        ShowBlogPosts.unId postId `shouldSatisfy` (> 0)

-- | A user who is not a host of the show gets NotAuthorized.
test_nonHostGetsNotAuthorized :: TestDBConfig -> IO ()
test_nonHostGetsNotAuthorized cfg = do
  -- Create a show with a host, then try as a different non-host user
  hostInsert <- mkUserInsert "blog-new-post-host" UserMetadata.Host
  nonHostInsert <- mkUserInsert "blog-new-post-nonhost" UserMetadata.User

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          hostId <- insertTestUser hostInsert
          (nonHostModel, nonHostMetaModel) <- setupUserModels nonHostInsert
          let showInsert = Shows.Insert "Blog New Post Other Show" (Slug "blog-new-post-other-show") Nothing Nothing Shows.Active
          (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
          addTestShowHost showId hostId
          pure (nonHostModel, nonHostMetaModel)
    (nonHostModel, nonHostMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action nonHostModel nonHostMetaModel (Slug "blog-new-post-other-show") sampleForm

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotAuthorized but got Right"
