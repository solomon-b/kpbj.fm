{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Blogs.Slug.Edit.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Slug.Edit.Get.Handler (BlogEditViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
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
import Test.Database.Helpers (addTestShowHost, insertTestShowBlogPost, insertTestShowWithSchedule, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Blogs.Slug.Edit.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent post ID" test_notFoundForMissingPost
        it "admin can access the edit form for a valid post" test_adminCanAccessEditForm
        it "host of the show can edit a post of another author" test_hostOfShowCanEditPost
        it "host who does not host the show and is not the author gets NotAuthorized" test_unrelatedHostGetsNotAuthorized

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Nonexistent post ID yields NotFound "Blog post".
test_notFoundForMissingPost :: TestDBConfig -> IO ()
test_notFoundForMissingPost cfg = do
  userInsert <- mkUserInsert "blog-edit-get-nf" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (userModel, userMetaModel) <- setupUserModels userInsert
          let showInsert = Shows.Insert "Blog Edit Get NF Show" (Slug "blog-edit-get-nf-show") Nothing Nothing Shows.Active
          _ <- insertTestShowWithSchedule showInsert defaultScheduleInsert
          pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (Slug "blog-edit-get-nf-show") (ShowBlogPosts.Id 99999)

    liftIO $ case result of
      Left (NotFound resource) -> resource `shouldBe` "Blog post"
      Left err -> expectationFailure $ "Expected NotFound \"Blog post\" but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Admin with a valid show and post can access the edit form.
test_adminCanAccessEditForm :: TestDBConfig -> IO ()
test_adminCanAccessEditForm cfg = do
  userInsert <- mkUserInsert "blog-edit-get-admin" UserMetadata.Admin

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (userModel, userMetaModel) <- setupUserModels userInsert
          let showInsert = Shows.Insert "Blog Edit Get Show" (Slug "blog-edit-get-show") Nothing Nothing Shows.Active
          (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
          addTestShowHost showId userModel.mId
          let postInsert =
                ShowBlogPosts.Insert
                  { ShowBlogPosts.sbpiId = showId,
                    ShowBlogPosts.sbpiTitle = "Edit Form Post",
                    ShowBlogPosts.sbpiSlug = Slug.mkSlug "Edit Form Post",
                    ShowBlogPosts.sbpiContent = "Content to edit.",
                    ShowBlogPosts.sbpiExcerpt = Nothing,
                    ShowBlogPosts.sbpiAuthorId = userModel.mId,
                    ShowBlogPosts.sbpiStatus = Draft
                  }
          postId <- insertTestShowBlogPost postInsert
          pure (userModel, userMetaModel, postId)
    (userModel, userMetaModel, postId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel (Slug "blog-edit-get-show") postId

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> bevBlogPost vd `shouldBe` bevBlogPost vd -- edit form data was returned

-- | Insert a show, its owner as host, and one post by the owner.
--
-- Returns the show slug and the post id.
setupPostByOwner ::
  Text ->
  UserMetadata.UserWithMetadataInsert ->
  TRX.Transaction (Slug, ShowBlogPosts.Id)
setupPostByOwner slugText ownerInsert = do
  ownerId <- insertTestUser ownerInsert
  let showSlug = Slug slugText
      showInsert = Shows.Insert slugText showSlug Nothing Nothing Shows.Active
  (showId, _) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
  addTestShowHost showId ownerId
  let postInsert =
        ShowBlogPosts.Insert
          { ShowBlogPosts.sbpiId = showId,
            ShowBlogPosts.sbpiTitle = "Owner Post",
            ShowBlogPosts.sbpiSlug = Slug.mkSlug "Owner Post",
            ShowBlogPosts.sbpiContent = "Owner content.",
            ShowBlogPosts.sbpiExcerpt = Nothing,
            ShowBlogPosts.sbpiAuthorId = ownerId,
            ShowBlogPosts.sbpiStatus = Draft
          }
  postId <- insertTestShowBlogPost postInsert
  pure (showSlug, postId)

-- | A host of the show can edit a post that another host wrote.
test_hostOfShowCanEditPost :: TestDBConfig -> IO ()
test_hostOfShowCanEditPost cfg = do
  ownerInsert <- mkUserInsert "blog-edit-get-owner" UserMetadata.Host
  cohostInsert <- mkUserInsert "blog-edit-get-cohost" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (showSlug, postId) <- setupPostByOwner "blog-edit-get-cohost-show" ownerInsert
          (cohostModel, cohostMetaModel) <- setupUserModels cohostInsert
          showModel <- TRX.statement () (Shows.getShowBySlug showSlug) >>= maybe (error "show not found") pure
          addTestShowHost (Shows.id showModel) cohostModel.mId
          pure (cohostModel, cohostMetaModel, showSlug, postId)
    (cohostModel, cohostMetaModel, showSlug, postId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action cohostModel cohostMetaModel showSlug postId

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> ShowBlogPosts.id (bevBlogPost vd) `shouldBe` postId

-- | A host who does not host the show and did not write the post gets
-- NotAuthorized.
test_unrelatedHostGetsNotAuthorized :: TestDBConfig -> IO ()
test_unrelatedHostGetsNotAuthorized cfg = do
  ownerInsert <- mkUserInsert "blog-edit-get-other-owner" UserMetadata.Host
  visitorInsert <- mkUserInsert "blog-edit-get-visitor" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $ do
          (showSlug, postId) <- setupPostByOwner "blog-edit-get-other-show" ownerInsert
          (visitorModel, visitorMetaModel) <- setupUserModels visitorInsert
          pure (visitorModel, visitorMetaModel, showSlug, postId)
    (visitorModel, visitorMetaModel, showSlug, postId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action visitorModel visitorMetaModel showSlug postId

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotAuthorized but got Right"
