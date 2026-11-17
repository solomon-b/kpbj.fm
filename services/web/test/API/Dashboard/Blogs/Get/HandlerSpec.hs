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
        it "host with two shows gets the show that the slug names" test_hostGetsShowNamedBySlug
        it "host given the slug of a show they do not host gets their own show" test_unrelatedSlugFallsBackToOwnShow

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
    (_userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel (Slug "blog-get-empty-show") Nothing

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

          pure userMetaModel
    userMetaModel <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel (Slug "blog-get-posts-show") Nothing

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
    (_userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    -- maybePage = Nothing should default to page 1
    result <- runExceptT $ action userMetaModel (Slug "blog-get-page-show") (Nothing :: Maybe Int64)

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> blvPage vd `shouldBe` 1

-- | A host of two shows gets the show that the slug names, not the first one.
test_hostGetsShowNamedBySlug :: TestDBConfig -> IO ()
test_hostGetsShowNamedBySlug cfg = do
  userInsert <- mkUserInsert "blog-get-two-shows" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (userModel, userMetaModel) <- setupUserModels userInsert
          let firstShowInsert = Shows.Insert "Blog Get Two A" (Slug "blog-get-two-a") Nothing Nothing Shows.Active
              secondShowInsert = Shows.Insert "Blog Get Two B" (Slug "blog-get-two-b") Nothing Nothing Shows.Active
          (firstShowId, _) <- insertTestShowWithSchedule firstShowInsert defaultScheduleInsert
          addTestShowHost firstShowId userModel.mId
          (secondShowId, _) <- insertTestShowWithSchedule secondShowInsert defaultScheduleInsert
          addTestShowHost secondShowId userModel.mId
          pure (userMetaModel, firstShowId, secondShowId)
    (userMetaModel, firstShowId, secondShowId) <- liftIO $ expectSetupRight dbResult

    resultA <- runExceptT $ action userMetaModel (Slug "blog-get-two-a") Nothing
    resultB <- runExceptT $ action userMetaModel (Slug "blog-get-two-b") Nothing

    liftIO $ do
      case resultA of
        Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
        Right vd -> fmap Shows.id (blvCurrentShow vd) `shouldBe` Just firstShowId
      case resultB of
        Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
        Right vd -> fmap Shows.id (blvCurrentShow vd) `shouldBe` Just secondShowId

-- | A host given the slug of a show they do not host gets their own show.
--
-- The slug only picks among the hosted shows. A slug outside that list falls
-- back to the first hosted show, so the page never displays the other show.
test_unrelatedSlugFallsBackToOwnShow :: TestDBConfig -> IO ()
test_unrelatedSlugFallsBackToOwnShow cfg = do
  userInsert <- mkUserInsert "blog-get-other-host" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (userModel, userMetaModel) <- setupUserModels userInsert
          let ownShowInsert = Shows.Insert "Blog Get Own Show" (Slug "blog-get-own-show") Nothing Nothing Shows.Active
              otherShowInsert = Shows.Insert "Blog Get Other Show" (Slug "blog-get-other-show") Nothing Nothing Shows.Active
          (ownShowId, _) <- insertTestShowWithSchedule ownShowInsert defaultScheduleInsert
          addTestShowHost ownShowId userModel.mId
          _ <- insertTestShowWithSchedule otherShowInsert defaultScheduleInsert
          pure (userMetaModel, ownShowId)
    (userMetaModel, ownShowId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel (Slug "blog-get-other-show") Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> fmap Shows.id (blvCurrentShow vd) `shouldBe` Just ownShowId
