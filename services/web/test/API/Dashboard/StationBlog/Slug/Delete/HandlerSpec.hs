module API.Dashboard.StationBlog.Slug.Delete.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Slug.Delete.Handler (action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestBlogPost, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.HUnit (assertFailure)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, nonExistentId)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.StationBlog.Slug.Delete.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent blog post ID" test_notFoundForMissingPost
        it "deletes a blog post from the DB" test_deletesStationBlogPost

--------------------------------------------------------------------------------

-- | Build a minimal blog post insert for testing.
mkBlogPostInsert :: Text -> User.Id -> BlogPosts.Insert
mkBlogPostInsert title authorId =
  BlogPosts.Insert
    { BlogPosts.bpiTitle = title,
      BlogPosts.bpiSlug = Slug.mkSlug title,
      BlogPosts.bpiContent = "Blog post content to be deleted.",
      BlogPosts.bpiExcerpt = Nothing,
      BlogPosts.bpiHeroImageUrl = Nothing,
      BlogPosts.bpiAuthorId = authorId,
      BlogPosts.bpiStatus = Published
    }

--------------------------------------------------------------------------------

-- | Calling action with a nonexistent BlogPosts.Id returns NotFound.
test_notFoundForMissingPost :: TestDBConfig -> IO ()
test_notFoundForMissingPost cfg = do
  bracketAppM cfg $ do
    result <- runExceptT $ action (BlogPosts.Id nonExistentId)

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | After calling action on an existing post, the post no longer exists in the DB.
test_deletesStationBlogPost :: TestDBConfig -> IO ()
test_deletesStationBlogPost cfg = do
  userInsert <- mkUserInsert "sb-delete-post" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      insertTestBlogPost (mkBlogPostInsert "Station Blog Post To Delete" userId)
    postId <- liftIO $ expectSetupRight dbResult

    -- Confirm the post exists before deletion.
    preDeleteResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (BlogPosts.getBlogPostById postId)
    liftIO $ do
      preDeleteResult' <- expectSetupRight preDeleteResult
      case preDeleteResult' of
        Nothing -> assertFailure "Expected post to exist before deletion"
        Just _ -> pure ()

    -- Delete the post.
    deleteResult <- runExceptT $ action postId

    liftIO $ case deleteResult of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right () -> pure ()

    -- Verify the post no longer exists in the DB.
    postDeleteResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (BlogPosts.getBlogPostById postId)

    liftIO $ do
      postDeleteResult' <- expectSetupRight postDeleteResult
      postDeleteResult' `shouldBe` Nothing
