module API.Dashboard.StationBlog.Slug.Edit.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Slug.Edit.Post.Handler (action)
import API.Dashboard.StationBlog.Slug.Edit.Post.Route (BlogEditForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug (..))
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestBlogPost, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, nonExistentId)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.StationBlog.Slug.Edit.Post.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent blog post ID" test_notFoundForMissingPost
        it "updates station blog post title in the DB" test_updatesStationBlogPost

--------------------------------------------------------------------------------

-- | Build a minimal blog post insert for testing.
mkBlogPostInsert :: Text -> User.Id -> BlogPosts.Insert
mkBlogPostInsert title authorId =
  BlogPosts.Insert
    { BlogPosts.bpiTitle = title,
      BlogPosts.bpiSlug = Slug.mkSlug title,
      BlogPosts.bpiContent = "Original blog post content.",
      BlogPosts.bpiExcerpt = Nothing,
      BlogPosts.bpiHeroImageUrl = Nothing,
      BlogPosts.bpiAuthorId = authorId,
      BlogPosts.bpiStatus = Published
    }

-- | Build a minimal valid edit form.
editForm :: Text -> BlogEditForm
editForm title =
  BlogEditForm
    { befTitle = title,
      befContent = "Updated blog post content.",
      befExcerpt = Nothing,
      befStatus = "published",
      befTags = [],
      befHeroImage = Nothing,
      befHeroImageClear = False
    }

--------------------------------------------------------------------------------

-- | Calling action with a nonexistent BlogPosts.Id returns NotFound.
test_notFoundForMissingPost :: TestDBConfig -> IO ()
test_notFoundForMissingPost cfg = do
  let slug = Slug "nonexistent-post-slug"
  let form = editForm "Any Title"

  bracketAppM cfg $ do
    result <- runExceptT $ action (BlogPosts.Id nonExistentId) slug form

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Updating an existing blog post changes its title in the DB.
test_updatesStationBlogPost :: TestDBConfig -> IO ()
test_updatesStationBlogPost cfg = do
  userInsert <- mkUserInsert "sb-edit-post-update" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      postId <- insertTestBlogPost (mkBlogPostInsert "Original Station Blog Title" userId)
      postModel <- TRX.statement () (BlogPosts.getBlogPostById postId) >>= maybe (error "post not found") pure
      pure (postId, postModel)
    (postId, postModel) <- liftIO $ expectSetupRight dbResult

    let newTitle = "Updated Station Blog Title"
    let form = editForm newTitle

    result <- runExceptT $ action postId (BlogPosts.bpmSlug postModel) form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    -- Verify the title was updated in the DB by fetching the post by its new slug.
    let updatedSlug = Slug.mkSlug newTitle
    updatedResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (BlogPosts.getBlogPostById postId)

    liftIO $ do
      updatedResult' <- expectSetupRight updatedResult
      case updatedResult' of
        Nothing -> expectationFailure "Expected updated post to exist in DB but got Nothing"
        Just updatedPost -> do
          BlogPosts.bpmTitle updatedPost `shouldBe` newTitle
          BlogPosts.bpmSlug updatedPost `shouldBe` updatedSlug
