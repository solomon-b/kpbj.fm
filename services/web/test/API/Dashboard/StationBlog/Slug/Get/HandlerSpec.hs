module API.Dashboard.StationBlog.Slug.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Slug.Get.Handler (StationBlogDetailViewData (..), action)
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
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestBlogPost)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, nonExistentId, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.StationBlog.Slug.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent blog post ID" test_notFoundForMissingPost
        it "returns post detail for an existing blog post" test_returnsPostDetail

--------------------------------------------------------------------------------

-- | Build a minimal blog post insert for testing.
mkBlogPostInsert :: Text -> User.Id -> BlogPosts.Insert
mkBlogPostInsert title authorId =
  BlogPosts.Insert
    { BlogPosts.bpiTitle = title,
      BlogPosts.bpiSlug = Slug.mkSlug title,
      BlogPosts.bpiContent = "Test blog post content.",
      BlogPosts.bpiExcerpt = Nothing,
      BlogPosts.bpiHeroImageUrl = Nothing,
      BlogPosts.bpiAuthorId = authorId,
      BlogPosts.bpiStatus = Published
    }

--------------------------------------------------------------------------------

-- | Calling action with a nonexistent BlogPosts.Id returns NotFound.
test_notFoundForMissingPost :: TestDBConfig -> IO ()
test_notFoundForMissingPost cfg = do
  userInsert <- mkUserInsert "sb-slug-get-notfound" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (BlogPosts.Id nonExistentId)

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Inserting a blog post and fetching by ID returns the correct post and author.
test_returnsPostDetail :: TestDBConfig -> IO ()
test_returnsPostDetail cfg = do
  userInsert <- mkUserInsert "sb-slug-get-detail" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      postId <- insertTestBlogPost (mkBlogPostInsert "Station Blog Slug Get Detail Post" userModel.mId)
      pure (userModel, userMetaModel, postId)
    (userModel, userMetaModel, postId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel postId

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        BlogPosts.bpmId (sbdvPost vd) `shouldBe` postId
        sbdvAuthor vd `shouldBe` Just userMetaModel
