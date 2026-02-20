{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Blogs.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Get.Handler (BlogListViewData (..), action)
import App.Handler.Error ()
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int64)
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
    describe "API.Dashboard.Blogs.Get.Handler" $ do
      describe "action" $ do
        it "returns empty list when staff user has no shows" test_emptyBlogListForNoShows
        it "returns blog posts for a show" test_returnsBlogPostsForShow
        it "defaults to page 1 when maybePage is Nothing" test_paginationDefaultsToPageOne

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Staff user with no shows gets an empty blog list.
test_emptyBlogListForNoShows :: TestDBConfig -> IO ()
test_emptyBlogListForNoShows cfg = do
  userInsert <- mkUserInsert "blog-get-empty" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (Slug "blog-get-empty-show") Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        blvCurrentShow vd `shouldBe` Nothing
        blvPosts vd `shouldBe` []
        blvHasMore vd `shouldBe` False

-- | Creates a show, makes the user a host, inserts a show blog post, and verifies it appears.
test_returnsBlogPostsForShow :: TestDBConfig -> IO ()
test_returnsBlogPostsForShow cfg = do
  userInsert <- mkUserInsert "blog-get-posts" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (userModel, userMetaModel) <- setupUserModels userInsert
          let showInsert = Shows.Insert "Blog Get Posts Show" (Slug "blog-get-posts-show") Nothing Nothing Shows.Active
          (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
          addTestShowHost showId userModel.mId

          let postInsert =
                ShowBlogPosts.Insert
                  { ShowBlogPosts.sbpiId = showId,
                    ShowBlogPosts.sbpiTitle = "Test Blog Post",
                    ShowBlogPosts.sbpiSlug = Slug.mkSlug "Test Blog Post",
                    ShowBlogPosts.sbpiContent = "Some content here.",
                    ShowBlogPosts.sbpiExcerpt = Nothing,
                    ShowBlogPosts.sbpiAuthorId = userModel.mId,
                    ShowBlogPosts.sbpiStatus = Published
                  }
          _ <- insertTestShowBlogPost postInsert

          pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (Slug "blog-get-posts-show") Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        length (blvPosts vd) `shouldBe` 1
        blvHasMore vd `shouldBe` False

-- | Calling action with Nothing for maybePage yields blvPage = 1.
test_paginationDefaultsToPageOne :: TestDBConfig -> IO ()
test_paginationDefaultsToPageOne cfg = do
  userInsert <- mkUserInsert "blog-get-page" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    -- maybePage = Nothing should default to page 1
    result <- runExceptT $ action userModel userMetaModel (Slug "blog-get-page-show") (Nothing :: Maybe Int64)

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> blvPage vd `shouldBe` 1
