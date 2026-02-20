module API.Shows.Slug.Blog.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Shows.Slug.Blog.Get.Handler (ShowBlogViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestShowBlogPost, insertTestShowWithSchedule, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.HUnit (assertFailure)
import Test.Handler.Fixtures (defaultScheduleInsert, mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Shows.Slug.Blog.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent show slug" test_notFoundForMissingShow
        it "returns empty posts list for a show with no blog posts" test_emptyBlogForNewShow
        it "returns inserted published blog post in posts list" test_returnsInsertedPosts

--------------------------------------------------------------------------------

-- | Construct a minimal show insert with a specific slug.
mkShowInsert :: Slug -> Shows.Insert
mkShowInsert slug =
  Shows.Insert
    { siTitle = "Test Show Blog",
      siSlug = slug,
      siDescription = Nothing,
      siLogoUrl = Nothing,
      siStatus = Shows.Active
    }

-- | Construct a show blog post insert for a given show and author.
mkShowBlogPostInsert :: Shows.Id -> User.Id -> ShowBlogPosts.Insert
mkShowBlogPostInsert showId userId =
  ShowBlogPosts.Insert
    { sbpiId = showId,
      sbpiTitle = "Test Show Blog Post",
      sbpiSlug = Slug "test-show-blog-post",
      sbpiContent = "Test content for show blog post.",
      sbpiExcerpt = Nothing,
      sbpiAuthorId = userId,
      sbpiStatus = Published
    }

--------------------------------------------------------------------------------

-- | Calling action with a nonexistent show slug returns a NotFound error.
test_notFoundForMissingShow :: TestDBConfig -> IO ()
test_notFoundForMissingShow cfg = bracketAppM cfg $ do
  let missingSlug = Slug "nonexistent-show-blog-xyz"
  result <- runExceptT $ action missingSlug Nothing Nothing
  liftIO $ case result of
    Left (NotFound _) -> pure ()
    Left err ->
      expectationFailure $ "Expected NotFound but got: " <> show err
    Right _ ->
      expectationFailure "Expected Left NotFound but got Right"

-- | A show with no blog posts returns an empty posts list.
test_emptyBlogForNewShow :: TestDBConfig -> IO ()
test_emptyBlogForNewShow cfg = bracketAppM cfg $ do
  let showSlug = Slug "test-show-blog-empty"

  dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    _ <- insertTestShowWithSchedule (mkShowInsert showSlug) defaultScheduleInsert
    pure ()

  case dbResult of
    Left err ->
      liftIO $ assertFailure $ "Test setup failed: " <> show err
    Right () -> do
      result <- runExceptT $ action showSlug Nothing Nothing
      liftIO $ case result of
        Left err ->
          expectationFailure $ "Expected Right but got Left: " <> show err
        Right vd ->
          sbvdPosts vd `shouldBe` []

-- | A published blog post inserted for a show appears in the posts list.
test_returnsInsertedPosts :: TestDBConfig -> IO ()
test_returnsInsertedPosts cfg = bracketAppM cfg $ do
  userInsert <- liftIO $ mkUserInsert "show-blog-get-test" UserMetadata.User
  let showSlug = Slug "test-show-blog-with-posts"

  dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    (showId, _) <- insertTestShowWithSchedule (mkShowInsert showSlug) defaultScheduleInsert
    insertTestShowBlogPost (mkShowBlogPostInsert showId userId)

  case dbResult of
    Left err ->
      liftIO $ assertFailure $ "Test setup failed: " <> show err
    Right postId -> do
      result <- runExceptT $ action showSlug Nothing Nothing
      liftIO $ case result of
        Left err ->
          expectationFailure $ "Expected Right but got Left: " <> show err
        Right vd -> do
          length (sbvdPosts vd) `shouldBe` 1
          case sbvdPosts vd of
            [post] -> ShowBlogPosts.id (post :: ShowBlogPosts.Model) `shouldBe` postId
            _ -> expectationFailure "Expected exactly one post"
