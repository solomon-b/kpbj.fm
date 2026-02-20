module API.Blog.Post.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Blog.Post.Get.Handler (BlogPostViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (mkSlug)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.User qualified as User
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog qualified as H
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestBlogPost, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight)
import Test.Gen.Tables.BlogPosts (blogPostInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Handler.Fixtures (expectSetupRight, nonExistentId)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Blog.Post.Get.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent post ID" test_notFoundForMissingPost

        runs 5 . it "returns BlogPostContent with correct slug" $
          hedgehog . prop_returnsContentWithCorrectSlug

        runs 5 . it "returns BlogPostRedirect with wrong slug" $
          hedgehog . prop_redirectsWithWrongSlug

--------------------------------------------------------------------------------

-- | A nonexistent post ID produces a NotFound error.
test_notFoundForMissingPost :: TestDBConfig -> IO ()
test_notFoundForMissingPost cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ action (BlogPosts.Id nonExistentId) (Just (mkSlug "does-not-exist"))
    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Calling action with the post's canonical slug returns BlogPostContent.
prop_returnsContentWithCorrectSlug :: TestDBConfig -> H.PropertyT IO ()
prop_returnsContentWithCorrectSlug cfg = do
  arrange (bracketAppM cfg) $ do
    userInsert <- forAllT userWithMetadataInsertGen
    postInsert <- forAllT $ blogPostInsertGen (User.Id 0)

    act $ do
      -- Setup: insert user and a Published blog post
      rawPostId <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userInsert
        let published = postInsert {BlogPosts.bpiStatus = Published, BlogPosts.bpiAuthorId = userId}
        insertTestBlogPost published
      insertedId <- expectSetupRight rawPostId

      -- Fetch the post to get its canonical slug; unwrap the Maybe via error on Nothing
      rawPost <-
        runDB $
          TRX.transaction TRX.ReadCommitted TRX.Read $
            TRX.statement () (BlogPosts.getBlogPostById insertedId)
      mPost <- expectSetupRight rawPost
      let post = case mPost of
            Nothing -> error "prop_returnsContentWithCorrectSlug: inserted post not found"
            Just p -> p
      let canonicalSlug = BlogPosts.bpmSlug post

      result <- runExceptT $ action insertedId (Just canonicalSlug)

      assert $ do
        vd <- assertRight result
        case vd of
          BlogPostContent _ blogPost _ _ ->
            H.assert (BlogPosts.bpmId blogPost == insertedId)
          BlogPostRedirect url -> do
            H.annotate $ "Expected BlogPostContent but got BlogPostRedirect to: " <> show url
            H.failure

-- | Calling action with a wrong slug returns BlogPostRedirect.
--
-- The sentinel slug "zzz-wrong-slug-sentinel-zzz" is longer than what genSlug
-- produces (max 20 chars) and contains a unique pattern it never generates,
-- ensuring a reliable slug mismatch.
prop_redirectsWithWrongSlug :: TestDBConfig -> H.PropertyT IO ()
prop_redirectsWithWrongSlug cfg = do
  arrange (bracketAppM cfg) $ do
    userInsert <- forAllT userWithMetadataInsertGen
    postInsert <- forAllT $ blogPostInsertGen (User.Id 0)

    act $ do
      -- Setup: insert user and a Published blog post
      rawPostId <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userInsert
        let published = postInsert {BlogPosts.bpiStatus = Published, BlogPosts.bpiAuthorId = userId}
        insertTestBlogPost published
      insertedId <- expectSetupRight rawPostId

      -- Use a fixed slug guaranteed not to match any generated canonical slug.
      -- genSlug generates prefix(3-10)-suffix(3-10) alphanumeric; "sentinel" is 8 chars
      -- but the overall pattern "zzz-wrong-slug-sentinel-zzz" is distinct and unreachable.
      let wrongSlug = mkSlug "zzz-wrong-slug-sentinel-zzz"

      result <- runExceptT $ action insertedId (Just wrongSlug)

      assert $ do
        vd <- assertRight result
        case vd of
          BlogPostRedirect _ -> pure ()
          BlogPostContent {} -> do
            H.annotate "Expected BlogPostRedirect but got BlogPostContent"
            H.failure
