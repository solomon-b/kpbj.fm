module API.Dashboard.StationBlog.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.Get.Handler (StationBlogListViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Domain.Types.HxRequest (HxRequest (..))
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestBlogPost)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.StationBlog.Get.Handler" $ do
      describe "action" $ do
        it "staff user on empty DB gets empty posts list" test_emptyListOnFreshDB
        it "returns inserted blog post in posts list" test_returnsBlogPostOnInsert
        it "pagination defaults to page 1 and isAppendRequest False" test_paginationDefaults

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

-- | Staff user calling action on an empty DB gets an empty posts list.
test_emptyListOnFreshDB :: TestDBConfig -> IO ()
test_emptyListOnFreshDB cfg = do
  userInsert <- mkUserInsert "sb-get-empty" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel Nothing IsNotHxRequest

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> sbvdPosts vd `shouldBe` []

-- | After inserting a blog post, it appears in the returned posts list.
test_returnsBlogPostOnInsert :: TestDBConfig -> IO ()
test_returnsBlogPostOnInsert cfg = do
  userInsert <- mkUserInsert "sb-get-insert" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      postId <- insertTestBlogPost (mkBlogPostInsert "Station Blog Get Test Post" userModel.mId)
      pure (userModel, userMetaModel, postId)
    (userModel, userMetaModel, postId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel Nothing IsNotHxRequest

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        length (sbvdPosts vd) `shouldBe` 1
        case sbvdPosts vd of
          [post] -> BlogPosts.bpmId post `shouldBe` postId
          _ -> expectationFailure "Expected exactly one post"

-- | Calling action with no page param defaults to page 1 and isAppendRequest False.
test_paginationDefaults :: TestDBConfig -> IO ()
test_paginationDefaults cfg = do
  userInsert <- mkUserInsert "sb-get-paginate" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel Nothing IsNotHxRequest

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        sbvdPage vd `shouldBe` 1
        sbvdIsAppendRequest vd `shouldBe` False
