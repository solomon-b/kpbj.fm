{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Blogs.Slug.Delete.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Slug.Delete.Handler (action)
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
import Test.Database.Helpers (addTestShowHost, insertTestShowBlogPost, insertTestShowWithSchedule)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

-- | The handler checks access to the show with
-- 'App.Handler.Combinators.authorizeShow' before 'action' runs.
spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Blogs.Slug.Delete.Handler" $ do
      describe "action" $ do
        it "deletes a show blog post" test_deletesShowBlogPost

--------------------------------------------------------------------------------

-- | The post is no longer retrievable from the DB after the delete.
test_deletesShowBlogPost :: TestDBConfig -> IO ()
test_deletesShowBlogPost cfg = do
  userInsert <- mkUserInsert "blog-delete-admin" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (userModel, _userMetaModel) <- setupUserModels userInsert
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
          showModel <- TRX.statement () (Shows.getShowById showId) >>= maybe (error "show not found") pure
          pure (showModel, postId)
    (showModel, postId) <- liftIO $ expectSetupRight dbResult

    -- Delete the post
    deleteResult <- runExceptT $ action showModel postId

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
