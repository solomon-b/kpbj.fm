module API.Blog.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Blog.Get.Handler (BlogListViewData (..), action)
import Control.Monad.Trans.Except (runExceptT)
import Data.Either (isRight)
import Data.Text qualified as Text
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.User qualified as User
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import Hedgehog.Range qualified as Range
import Test.Database.Helpers (insertTestBlogPost, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight)
import Test.Gen.Tables.BlogPosts (blogPostInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Handler.Fixtures (expectSetupRight)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Blog.Get.Handler" $ do
      describe "action" $ do
        runs 1 . it "returns empty posts list on empty DB" $
          hedgehog . prop_emptyDB

        runs 5 . it "always returns Right" $
          hedgehog . prop_returnsRight

        runs 5 . it "returns inserted Published posts" $
          hedgehog . prop_returnsInsertedPosts

        runs 1 . it "pagination: hasMore True on page 1, remainder on page 2" $
          hedgehog . prop_pagination

--------------------------------------------------------------------------------

-- | Append an index to a slug to guarantee uniqueness across a list of inserts.
-- Prevents duplicate-slug failures when Hedgehog shrinks all generated values
-- to the same minimum.
uniquifySlug :: Int -> BlogPosts.Insert -> BlogPosts.Insert
uniquifySlug idx p =
  let Slug s = BlogPosts.bpiSlug p
   in p {BlogPosts.bpiSlug = Slug (s <> "-" <> Text.pack (show idx))}

-- | Empty DB returns empty posts, hasMore False, on page 1.
prop_emptyDB :: TestDBConfig -> PropertyT IO ()
prop_emptyDB cfg = do
  arrange (bracketAppM cfg) $ do
    act $ do
      result <- runExceptT $ action Nothing Nothing
      assert $ do
        vd <- assertRight result
        blvdPosts vd === []
        blvdHasMore vd === False

-- | Action always returns Right on a valid database.
prop_returnsRight :: TestDBConfig -> PropertyT IO ()
prop_returnsRight cfg = do
  arrange (bracketAppM cfg) $ do
    act $ do
      result <- runExceptT $ action Nothing Nothing
      assert $ do
        H.assert (isRight result)

-- | Inserting Published blog posts causes them to appear in the result.
--
-- Note: only Published posts are shown on the public blog list.
-- Post inserts are generated with a placeholder userId, then the real
-- userId is substituted inside the transaction.
prop_returnsInsertedPosts :: TestDBConfig -> PropertyT IO ()
prop_returnsInsertedPosts cfg = do
  arrange (bracketAppM cfg) $ do
    userInsert <- forAllT userWithMetadataInsertGen
    postInserts <- forAllT $ Gen.list (Range.linear 1 5) (blogPostInsertGen (User.Id 0))

    act $ do
      -- Count posts before insert
      beforeResult <- runExceptT $ action Nothing Nothing
      let beforeCount = either (const 0) (length . blvdPosts) beforeResult

      -- Setup: insert user, then insert Published blog posts with the real userId.
      -- Slugs are uniquified to prevent duplicate-key failures on shrink.
      dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userInsert
        let published = zipWith (\idx p -> (uniquifySlug idx p) {BlogPosts.bpiStatus = Published, BlogPosts.bpiAuthorId = userId}) [0 :: Int ..] postInserts
        mapM_ insertTestBlogPost published
      expectSetupRight dbResult

      afterResult <- runExceptT $ action Nothing Nothing

      assert $ do
        vd <- assertRight afterResult
        length (blvdPosts vd) === beforeCount + length postInserts

-- | When more than 10 posts exist, hasMore is True on page 1 and page 2 has the remainder.
--
-- The action uses a page size of 10: it fetches limit+1 posts to detect hasMore.
prop_pagination :: TestDBConfig -> PropertyT IO ()
prop_pagination cfg = do
  arrange (bracketAppM cfg) $ do
    userInsert <- forAllT userWithMetadataInsertGen
    -- Generate 11 posts (one more than the page size of 10)
    postInserts <- forAllT $ Gen.list (Range.singleton 11) (blogPostInsertGen (User.Id 0))

    act $ do
      -- Setup: insert user and 11 Published blog posts.
      -- Slugs are uniquified to prevent duplicate-key failures on shrink.
      dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userInsert
        let published = zipWith (\idx p -> (uniquifySlug idx p) {BlogPosts.bpiStatus = Published, BlogPosts.bpiAuthorId = userId}) [0 :: Int ..] postInserts
        mapM_ insertTestBlogPost published
      expectSetupRight dbResult

      -- Page 1: should have 10 posts and hasMore = True
      result1 <- runExceptT $ action (Just 1) Nothing
      -- Page 2: should have the 11th post
      result2 <- runExceptT $ action (Just 2) Nothing

      assert $ do
        vd1 <- assertRight result1
        blvdHasMore vd1 === True
        length (blvdPosts vd1) === 10

        vd2 <- assertRight result2
        H.assert (not (null (blvdPosts vd2)))
