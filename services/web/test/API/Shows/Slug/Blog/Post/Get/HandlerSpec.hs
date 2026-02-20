module API.Shows.Slug.Blog.Post.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Shows.Slug.Blog.Post.Get.Handler (ShowBlogPostOutcome (..), ShowBlogPostViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text qualified as Text
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestShowBlogPost, insertTestShowWithSchedule, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.HUnit (assertFailure)
import Test.Handler.Fixtures (mkUserInsert, nonExistentId)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Shows.Slug.Blog.Post.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent blog post ID" test_notFoundForMissingPost
        it "returns RenderPost when the slug matches the canonical slug" test_renderPostWithCorrectSlug
        it "returns RedirectTo when the slug does not match" test_redirectWithWrongSlug
        it "returns NotFound when showId does not match the post's show" test_notFoundWrongShowId

--------------------------------------------------------------------------------

-- | Construct a minimal show insert with a specific slug.
mkShowInsert :: Slug -> Shows.Insert
mkShowInsert slug =
  Shows.Insert
    { siTitle = "Test Show Blog Post",
      siSlug = slug,
      siDescription = Nothing,
      siLogoUrl = Nothing,
      siStatus = Shows.Active
    }

-- | Construct a schedule template insert (placeholder showId overridden by helper).
mkScheduleTemplate :: ShowSchedule.ScheduleTemplateInsert
mkScheduleTemplate =
  ShowSchedule.ScheduleTemplateInsert
    { stiShowId = Shows.Id 0,
      stiDayOfWeek = Nothing,
      stiWeeksOfMonth = Nothing,
      stiStartTime = read "10:00:00",
      stiEndTime = read "12:00:00",
      stiTimezone = "America/Los_Angeles",
      stiAirsTwiceDaily = False
    }

-- | Construct a show blog post insert with a fixed canonical slug.
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

-- | Calling action with a nonexistent post ID returns a NotFound error.
test_notFoundForMissingPost :: TestDBConfig -> IO ()
test_notFoundForMissingPost cfg = bracketAppM cfg $ do
  result <- runExceptT $ action (Shows.Id nonExistentId) (ShowBlogPosts.Id nonExistentId) (Just (Slug "any-slug"))
  liftIO $ case result of
    Left (NotFound _) -> pure ()
    Left err ->
      expectationFailure $ "Expected NotFound but got: " <> show err
    Right _ ->
      expectationFailure "Expected Left NotFound but got Right"

-- | When the URL slug matches the canonical slug, RenderPost is returned.
test_renderPostWithCorrectSlug :: TestDBConfig -> IO ()
test_renderPostWithCorrectSlug cfg = bracketAppM cfg $ do
  userInsert <- liftIO $ mkUserInsert "show-blog-post-get-test" UserMetadata.User
  let showSlug = Slug "test-show-blog-post-correct"
      canonicalPostSlug = Slug "test-show-blog-post"

  dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    (showId, _) <- insertTestShowWithSchedule (mkShowInsert showSlug) mkScheduleTemplate
    postId <- insertTestShowBlogPost (mkShowBlogPostInsert showId userId)
    pure (showId, postId)

  case dbResult of
    Left err ->
      liftIO $ assertFailure $ "Test setup failed: " <> show err
    Right (showId, postId) -> do
      result <- runExceptT $ action showId postId (Just canonicalPostSlug)
      liftIO $ case result of
        Left err ->
          expectationFailure $ "Expected Right but got Left: " <> show err
        Right (RedirectTo url) ->
          expectationFailure $ "Expected RenderPost but got RedirectTo: " <> show url
        Right (RenderPost vd) ->
          ShowBlogPosts.id (sbpvdPost vd :: ShowBlogPosts.Model) `shouldBe` postId

-- | When the URL slug does not match the canonical slug, RedirectTo is returned
-- with a URL containing the canonical slug.
test_redirectWithWrongSlug :: TestDBConfig -> IO ()
test_redirectWithWrongSlug cfg = bracketAppM cfg $ do
  userInsert <- liftIO $ mkUserInsert "show-blog-post-get-test" UserMetadata.User
  let showSlug = Slug "test-show-blog-post-redirect"
      canonicalPostSlug = Slug "test-show-blog-post"
      wrongSlug = Slug "wrong-slug-here"

  dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    (showId, _) <- insertTestShowWithSchedule (mkShowInsert showSlug) mkScheduleTemplate
    postId <- insertTestShowBlogPost (mkShowBlogPostInsert showId userId)
    pure (showId, postId)

  case dbResult of
    Left err ->
      liftIO $ assertFailure $ "Test setup failed: " <> show err
    Right (showId, postId) -> do
      result <- runExceptT $ action showId postId (Just wrongSlug)
      liftIO $ case result of
        Left err ->
          expectationFailure $ "Expected Right but got Left: " <> show err
        Right (RenderPost _) ->
          expectationFailure "Expected RedirectTo but got RenderPost"
        Right (RedirectTo url) -> do
          url `shouldSatisfy` Text.isInfixOf "test-show-blog-post"
          url `shouldSatisfy` (not . Text.isInfixOf "wrong-slug-here")
          -- The redirect URL must contain the canonical post slug, not the wrong one
          let canonicalSlugText = let Slug t = canonicalPostSlug in t
          url `shouldSatisfy` Text.isInfixOf canonicalSlugText

-- | When the showId passed to action does not match the post's show, NotFound is returned.
test_notFoundWrongShowId :: TestDBConfig -> IO ()
test_notFoundWrongShowId cfg = bracketAppM cfg $ do
  userInsert <- liftIO $ mkUserInsert "show-blog-post-get-test" UserMetadata.User
  let showSlug = Slug "test-show-blog-post-wrong-show"
      canonicalPostSlug = Slug "test-show-blog-post"

  dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    (showId, _) <- insertTestShowWithSchedule (mkShowInsert showSlug) mkScheduleTemplate
    postId <- insertTestShowBlogPost (mkShowBlogPostInsert showId userId)
    pure (showId, postId)

  case dbResult of
    Left err ->
      liftIO $ assertFailure $ "Test setup failed: " <> show err
    Right (_actualShowId, postId) -> do
      let differentShowId = Shows.Id 999998
      result <- runExceptT $ action differentShowId postId (Just canonicalPostSlug)
      liftIO $ case result of
        Left (NotFound _) -> pure ()
        Left err ->
          expectationFailure $ "Expected NotFound but got: " <> show err
        Right _ ->
          expectationFailure "Expected Left NotFound but got Right"
