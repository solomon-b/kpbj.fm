module API.Dashboard.StationBlog.New.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.New.Post.Handler (action)
import API.Dashboard.StationBlog.New.Post.Route (NewBlogPostForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.List (find)
import Data.Maybe (isJust)
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
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.StationBlog.New.Post.Handler" $ do
      describe "action" $ do
        it "staff user creates a station blog post" test_createsStationBlogPost
        it "staff user creates a post with tags" test_createsPostWithTags
        it "returns ValidationError for duplicate slug" test_validationErrorForDuplicateSlug

--------------------------------------------------------------------------------

-- | A minimal valid form for creating a new station blog post.
minimalForm :: Text -> NewBlogPostForm
minimalForm title =
  NewBlogPostForm
    { nbpfTitle = title,
      nbpfContent = "Station blog post content for testing.",
      nbpfExcerpt = Nothing,
      nbpfStatus = Just "published",
      nbpfTags = [],
      nbpfHeroImage = Nothing
    }

--------------------------------------------------------------------------------

-- | Staff user creates a station blog post; the post appears in the DB afterwards.
test_createsStationBlogPost :: TestDBConfig -> IO ()
test_createsStationBlogPost cfg = do
  userInsert <- mkUserInsert "sb-new-post-create" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
    userModel <- liftIO $ expectSetupRight dbResult

    let form = minimalForm "Station Blog New Post Test"
    result <- runExceptT $ action userModel form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    -- Verify the post was persisted in the DB by looking it up via its generated slug.
    let expectedSlug = Slug.mkSlug "Station Blog New Post Test"
    postResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (BlogPosts.getAllBlogPosts 20 0)

    liftIO $ do
      postResult' <- expectSetupRight postResult
      postResult' `shouldSatisfy` any (\p -> BlogPosts.bpmSlug p == expectedSlug)

-- | Staff user creates a post with tags and the post is persisted.
test_createsPostWithTags :: TestDBConfig -> IO ()
test_createsPostWithTags cfg = do
  userInsert <- mkUserInsert "sb-new-post-tags" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
    userModel <- liftIO $ expectSetupRight dbResult

    let form =
          (minimalForm "Station Blog Post With Tags")
            { nbpfTags = ["haskell", "radio", "community"]
            }
    result <- runExceptT $ action userModel form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    -- Verify the post exists and has the expected tags.
    let expectedSlug = Slug.mkSlug "Station Blog Post With Tags"
    postResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ do
      posts <- TRX.statement () (BlogPosts.getAllBlogPosts 20 0)
      let mPost = find (\p -> BlogPosts.bpmSlug p == expectedSlug) posts
      case mPost of
        Nothing -> pure (Nothing, [])
        Just post -> do
          tags <- TRX.statement () (BlogPosts.getTagsForPost (BlogPosts.bpmId post))
          pure (Just post, tags)

    liftIO $ do
      (mPost, tags) <- expectSetupRight postResult
      mPost `shouldSatisfy` isJust
      length tags `shouldBe` 3

-- | Creating two blog posts with the same title (same derived slug) returns
-- a ValidationError rather than an opaque DatabaseError.
test_validationErrorForDuplicateSlug :: TestDBConfig -> IO ()
test_validationErrorForDuplicateSlug cfg = do
  userInsert <- mkUserInsert "sb-new-post-dup-slug" UserMetadata.Staff

  bracketAppM cfg $ do
    -- Pre-seed a blog post with the slug that the form will derive.
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      userModel <- TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
      let postInsert =
            BlogPosts.Insert
              { BlogPosts.bpiTitle = "Duplicate Slug Post",
                BlogPosts.bpiSlug = Slug.mkSlug "Duplicate Slug Post",
                BlogPosts.bpiContent = "Existing post content.",
                BlogPosts.bpiExcerpt = Nothing,
                BlogPosts.bpiHeroImageUrl = Nothing,
                BlogPosts.bpiAuthorId = userId,
                BlogPosts.bpiStatus = Published
              }
      _ <- insertTestBlogPost postInsert
      pure userModel
    userModel <- liftIO $ expectSetupRight dbResult

    -- Attempt to create a post with the same title (same slug).
    let form = minimalForm "Duplicate Slug Post"
    result <- runExceptT $ action userModel form

    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"
