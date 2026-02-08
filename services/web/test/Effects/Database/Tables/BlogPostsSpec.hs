module Effects.Database.Tables.BlogPostsSpec where

--------------------------------------------------------------------------------

import Control.Monad (void)
import Data.Either (isLeft)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BlogPosts qualified as UUT
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.User qualified as User
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, assertSingleton, (->-), (<==), (=\\=))
import Test.Gen.Tables.BlogPosts (blogPostInsertGen)
import Test.Gen.Tables.BlogTags (blogTagInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.BlogPosts" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: inserted fields preserved on select" $
          hedgehog . prop_insertSelect
        runs 10 . it "update-select: updated fields overwrite original on select" $
          hedgehog . prop_updateSelect
        runs 10 . it "update-update: second update fully overwrites first" $
          hedgehog . prop_updateUpdate

      describe "Queries" $ do
        describe "getAllBlogPosts" $ do
          runs 10 . it "returns all posts regardless of status" $ hedgehog . prop_getAllBlogPosts
          runs 10 . it "respects Limit" $ hedgehog . prop_getAllBlogPosts_limit
          runs 10 . it "respects Offset" $ hedgehog . prop_getAllBlogPosts_offset
        describe "getPublishedBlogPosts" $ do
          runs 10 . it "filters by Published status" $ hedgehog . prop_getPublishedBlogPosts
        describe "updateBlogPost" $ do
          runs 10 . it "manages published_at transitions" $ hedgehog . prop_updateBlogPost_publishedAt
          runs 10 . it "rejects duplicate slug" $ hedgehog . prop_updateBlogPost_duplicateSlug
        describe "deleteBlogPost" $ do
          runs 10 . it "removes post and delete is idempotent" $ hedgehog . prop_deleteBlogPost
          runs 10 . it "cascades to blog_post_tags" $ hedgehog . prop_deleteBlogPost_cascade
        describe "Tag Operations" $ do
          runs 10 . it "addTagToPost and getTagsForPost" $ hedgehog . prop_addAndGetTagsForPost
          runs 10 . it "removeTagFromPost removes tag" $ hedgehog . prop_removeTagFromPost
          runs 10 . it "getPostsByTag returns only published posts" $ hedgehog . prop_getPostsByTag
          runs 10 . it "getPostsByTag respects Limit" $ hedgehog . prop_getPostsByTag_limit
          runs 10 . it "getPostsByTag respects Offset" $ hedgehog . prop_getPostsByTag_offset

--------------------------------------------------------------------------------
-- Helpers

-- | Assert all user-provided fields in an Insert match the corresponding Model fields.
assertInsertFieldsMatch :: UUT.Insert -> UUT.Model -> PropertyT IO ()
assertInsertFieldsMatch insert model = do
  UUT.bpiTitle insert === UUT.bpmTitle model
  UUT.bpiSlug insert === UUT.bpmSlug model
  UUT.bpiContent insert === UUT.bpmContent model
  UUT.bpiExcerpt insert === UUT.bpmExcerpt model
  UUT.bpiHeroImageUrl insert === UUT.bpmHeroImageUrl model
  UUT.bpiAuthorId insert === UUT.bpmAuthorId model
  UUT.bpiStatus insert === UUT.bpmStatus model

--------------------------------------------------------------------------------
-- Lens Laws

-- | Insert-Select (PutGet): insert then select returns what we inserted.
prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    blogPostTemplate <- forAllT $ blogPostInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let blogPostInsert = blogPostTemplate {UUT.bpiAuthorId = userId}

        postId <- unwrapInsert (UUT.insertBlogPost blogPostInsert)
        selected <- TRX.statement () (UUT.getBlogPostById postId)
        TRX.condemn
        pure (postId, blogPostInsert, selected)

      assert $ do
        (postId, blogPostInsert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch blogPostInsert selected
        UUT.bpmId selected === postId
        postId ->- UUT.Id 0
        case UUT.bpiStatus blogPostInsert of
          Published -> void $ assertJust (UUT.bpmPublishedAt selected)
          _ -> UUT.bpmPublishedAt selected === Nothing
        pure ()

-- | Update-Select (PutGet for updates): update then select returns updated values.
prop_updateSelect :: TestDBConfig -> PropertyT IO ()
prop_updateSelect cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    originalTemplate <- forAllT $ blogPostInsertGen (User.Id 1)
    updateTemplate <- forAllT $ blogPostInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let original = originalTemplate {UUT.bpiAuthorId = userId, UUT.bpiStatus = Draft}
        postId <- unwrapInsert (UUT.insertBlogPost original)

        let updated = updateTemplate {UUT.bpiAuthorId = userId, UUT.bpiStatus = Draft}
        updateResult <- TRX.statement () (UUT.updateBlogPost postId updated)

        selected <- TRX.statement () (UUT.getBlogPostById postId)
        TRX.condemn
        pure (postId, updated, updateResult, selected)

      assert $ do
        (postId, updated, updateResult, mSelected) <- assertRight result
        updatedId <- assertJust updateResult
        updatedId === postId

        selected <- assertJust mSelected
        assertInsertFieldsMatch updated selected
        UUT.bpmId selected === postId
        UUT.bpmPublishedAt selected === Nothing
        pure ()

-- | Update-Update (PutPut): two successive updates; final state matches only the second.
prop_updateUpdate :: TestDBConfig -> PropertyT IO ()
prop_updateUpdate cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    originalTemplate <- forAllT $ blogPostInsertGen (User.Id 1)
    updateATemplate <- forAllT $ blogPostInsertGen (User.Id 1)
    updateBTemplate <- forAllT $ blogPostInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let original = originalTemplate {UUT.bpiAuthorId = userId, UUT.bpiStatus = Draft}
        postId <- unwrapInsert (UUT.insertBlogPost original)

        let updateA = updateATemplate {UUT.bpiAuthorId = userId, UUT.bpiStatus = Draft}
        _ <- TRX.statement () (UUT.updateBlogPost postId updateA)

        let updateB = updateBTemplate {UUT.bpiAuthorId = userId, UUT.bpiStatus = Draft}
        _ <- TRX.statement () (UUT.updateBlogPost postId updateB)

        selected <- TRX.statement () (UUT.getBlogPostById postId)
        TRX.condemn
        pure (postId, updateB, selected)

      assert $ do
        (postId, updateB, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch updateB selected
        UUT.bpmId selected === postId
        UUT.bpmPublishedAt selected === Nothing
        pure ()

--------------------------------------------------------------------------------
-- Query tests

prop_getAllBlogPosts :: TestDBConfig -> PropertyT IO ()
prop_getAllBlogPosts cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ blogPostInsertGen (User.Id 1)
    template2 <- forAllT $ blogPostInsertGen (User.Id 1)
    template3 <- forAllT $ blogPostInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let post1 = template1 {UUT.bpiAuthorId = userId, UUT.bpiStatus = Draft, UUT.bpiSlug = UUT.bpiSlug template1 <> Slug "1"}
        let post2 = template2 {UUT.bpiAuthorId = userId, UUT.bpiStatus = Published, UUT.bpiSlug = UUT.bpiSlug template2 <> Slug "2"}
        let post3 = template3 {UUT.bpiAuthorId = userId, UUT.bpiStatus = Deleted, UUT.bpiSlug = UUT.bpiSlug template3 <> Slug "3"}

        id1 <- unwrapInsert (UUT.insertBlogPost post1)
        id2 <- unwrapInsert (UUT.insertBlogPost post2)
        id3 <- unwrapInsert (UUT.insertBlogPost post3)

        allPosts <- TRX.statement () (UUT.getAllBlogPosts (Limit 10) (Offset 0))
        TRX.condemn
        pure (id1, id2, id3, allPosts)

      assert $ do
        (id1, id2, id3, allPosts) <- assertRight result
        -- All 3 posts returned regardless of status
        length allPosts === 3
        let returnedIds = map UUT.bpmId allPosts
        returnedIds =\\= [id1, id2, id3]
        pure ()

prop_getAllBlogPosts_limit :: TestDBConfig -> PropertyT IO ()
prop_getAllBlogPosts_limit cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ blogPostInsertGen (User.Id 1)
    template2 <- forAllT $ blogPostInsertGen (User.Id 1)
    template3 <- forAllT $ blogPostInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let post1 = template1 {UUT.bpiAuthorId = userId, UUT.bpiSlug = UUT.bpiSlug template1 <> Slug "1"}
        let post2 = template2 {UUT.bpiAuthorId = userId, UUT.bpiSlug = UUT.bpiSlug template2 <> Slug "2"}
        let post3 = template3 {UUT.bpiAuthorId = userId, UUT.bpiSlug = UUT.bpiSlug template3 <> Slug "3"}

        _ <- unwrapInsert (UUT.insertBlogPost post1)
        _ <- unwrapInsert (UUT.insertBlogPost post2)
        _ <- unwrapInsert (UUT.insertBlogPost post3)

        limited <- TRX.statement () (UUT.getAllBlogPosts (Limit 2) (Offset 0))
        TRX.condemn
        pure limited

      assert $ do
        limited <- assertRight result
        length limited === 2
        pure ()

prop_getAllBlogPosts_offset :: TestDBConfig -> PropertyT IO ()
prop_getAllBlogPosts_offset cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ blogPostInsertGen (User.Id 1)
    template2 <- forAllT $ blogPostInsertGen (User.Id 1)
    template3 <- forAllT $ blogPostInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let post1 = template1 {UUT.bpiAuthorId = userId, UUT.bpiSlug = UUT.bpiSlug template1 <> Slug "1"}
        let post2 = template2 {UUT.bpiAuthorId = userId, UUT.bpiSlug = UUT.bpiSlug template2 <> Slug "2"}
        let post3 = template3 {UUT.bpiAuthorId = userId, UUT.bpiSlug = UUT.bpiSlug template3 <> Slug "3"}

        _ <- unwrapInsert (UUT.insertBlogPost post1)
        _ <- unwrapInsert (UUT.insertBlogPost post2)
        _ <- unwrapInsert (UUT.insertBlogPost post3)

        allPosts <- TRX.statement () (UUT.getAllBlogPosts (Limit 10) (Offset 0))
        offset1 <- TRX.statement () (UUT.getAllBlogPosts (Limit 10) (Offset 1))
        offset2 <- TRX.statement () (UUT.getAllBlogPosts (Limit 10) (Offset 2))
        offset3 <- TRX.statement () (UUT.getAllBlogPosts (Limit 10) (Offset 3))
        TRX.condemn
        pure (allPosts, offset1, offset2, offset3)

      assert $ do
        (allPosts, offset1, offset2, offset3) <- assertRight result
        length allPosts === 3
        length offset1 === 2
        length offset2 === 1
        length offset3 === 0
        pure ()

prop_getPublishedBlogPosts :: TestDBConfig -> PropertyT IO ()
prop_getPublishedBlogPosts cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ blogPostInsertGen (User.Id 1)
    template2 <- forAllT $ blogPostInsertGen (User.Id 1)
    template3 <- forAllT $ blogPostInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let publishedPost = template1 {UUT.bpiAuthorId = userId, UUT.bpiStatus = Published, UUT.bpiSlug = UUT.bpiSlug template1 <> Slug "1"}
        let draftPost = template2 {UUT.bpiAuthorId = userId, UUT.bpiStatus = Draft, UUT.bpiSlug = UUT.bpiSlug template2 <> Slug "2"}
        let deletedPost = template3 {UUT.bpiAuthorId = userId, UUT.bpiStatus = Deleted, UUT.bpiSlug = UUT.bpiSlug template3 <> Slug "3"}

        publishedId <- unwrapInsert (UUT.insertBlogPost publishedPost)
        _draftId <- unwrapInsert (UUT.insertBlogPost draftPost)
        _deletedId <- unwrapInsert (UUT.insertBlogPost deletedPost)

        published <- TRX.statement () (UUT.getPublishedBlogPosts (Limit 10) (Offset 0))
        TRX.condemn
        pure (publishedId, published)

      assert $ do
        (publishedId, published) <- assertRight result
        post <- assertSingleton published
        UUT.bpmId post === publishedId
        UUT.bpmStatus post === Published
        pure ()

prop_updateBlogPost_publishedAt :: TestDBConfig -> PropertyT IO ()
prop_updateBlogPost_publishedAt cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template <- forAllT $ blogPostInsertGen (User.Id 1)
    updateTemplate <- forAllT $ blogPostInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        -- Insert as Draft
        let draftPost = template {UUT.bpiAuthorId = userId, UUT.bpiStatus = Draft}
        postId <- unwrapInsert (UUT.insertBlogPost draftPost)

        -- Transition Draft -> Published: published_at should be set
        let publishUpdate = updateTemplate {UUT.bpiAuthorId = userId, UUT.bpiStatus = Published}
        _ <- TRX.statement () (UUT.updateBlogPost postId publishUpdate)
        afterPublish <- TRX.statement () (UUT.getBlogPostById postId)

        -- Update Published -> Published: published_at should remain the same
        let stayPublished = updateTemplate {UUT.bpiAuthorId = userId, UUT.bpiStatus = Published}
        _ <- TRX.statement () (UUT.updateBlogPost postId stayPublished)
        afterStayPublished <- TRX.statement () (UUT.getBlogPostById postId)

        -- Transition Published -> Draft: published_at should be cleared
        let backToDraft = updateTemplate {UUT.bpiAuthorId = userId, UUT.bpiStatus = Draft}
        _ <- TRX.statement () (UUT.updateBlogPost postId backToDraft)
        afterBackToDraft <- TRX.statement () (UUT.getBlogPostById postId)

        TRX.condemn
        pure (afterPublish, afterStayPublished, afterBackToDraft)

      assert $ do
        (mAfterPublish, mAfterStayPublished, mAfterBackToDraft) <- assertRight result

        afterPublish <- assertJust mAfterPublish
        publishedAt1 <- assertJust (UUT.bpmPublishedAt afterPublish)

        afterStayPublished <- assertJust mAfterStayPublished
        publishedAt2 <- assertJust (UUT.bpmPublishedAt afterStayPublished)
        -- published_at should not change when staying Published
        publishedAt1 === publishedAt2

        afterBackToDraft <- assertJust mAfterBackToDraft
        UUT.bpmPublishedAt afterBackToDraft === Nothing
        pure ()

prop_updateBlogPost_duplicateSlug :: TestDBConfig -> PropertyT IO ()
prop_updateBlogPost_duplicateSlug cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ blogPostInsertGen (User.Id 1)
    template2 <- forAllT $ blogPostInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let post1 = template1 {UUT.bpiAuthorId = userId, UUT.bpiSlug = UUT.bpiSlug template1 <> Slug "1"}
        let post2 = template2 {UUT.bpiAuthorId = userId, UUT.bpiSlug = UUT.bpiSlug template2 <> Slug "2"}

        _id1 <- unwrapInsert (UUT.insertBlogPost post1)
        id2 <- unwrapInsert (UUT.insertBlogPost post2)

        -- Update post2's slug to match post1's slug
        let duplicateSlug = post1 {UUT.bpiAuthorId = userId}
        _ <- TRX.statement () (UUT.updateBlogPost id2 duplicateSlug)
        TRX.condemn
        pure ()

      assert $ do
        result <== isLeft
        pure ()

prop_deleteBlogPost :: TestDBConfig -> PropertyT IO ()
prop_deleteBlogPost cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template <- forAllT $ blogPostInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let post = template {UUT.bpiAuthorId = userId}
        postId <- unwrapInsert (UUT.insertBlogPost post)

        -- Delete returns Just id
        deleteResult <- TRX.statement () (UUT.deleteBlogPost postId)

        -- Post should be gone
        afterDelete <- TRX.statement () (UUT.getBlogPostById postId)

        -- Deleting again returns Nothing
        deleteAgain <- TRX.statement () (UUT.deleteBlogPost postId)

        TRX.condemn
        pure (postId, deleteResult, afterDelete, deleteAgain)

      assert $ do
        (postId, deleteResult, afterDelete, deleteAgain) <- assertRight result
        deletedId <- assertJust deleteResult
        deletedId === postId
        assertNothing afterDelete
        assertNothing deleteAgain
        pure ()

prop_deleteBlogPost_cascade :: TestDBConfig -> PropertyT IO ()
prop_deleteBlogPost_cascade cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template <- forAllT $ blogPostInsertGen (User.Id 1)
    tagInsert <- forAllT blogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let post = template {UUT.bpiAuthorId = userId}
        postId <- unwrapInsert (UUT.insertBlogPost post)

        tagId <- unwrapInsert (BlogTags.insertTag tagInsert)
        TRX.statement () (UUT.addTagToPost postId tagId)

        -- Verify tag is attached
        tagsBefore <- TRX.statement () (UUT.getTagsForPost postId)

        -- Delete the post
        _ <- TRX.statement () (UUT.deleteBlogPost postId)

        -- Junction rows should be gone (CASCADE)
        tagsAfter <- TRX.statement () (UUT.getTagsForPost postId)

        TRX.condemn
        pure (tagsBefore, tagsAfter)

      assert $ do
        (tagsBefore, tagsAfter) <- assertRight result
        length tagsBefore === 1
        length tagsAfter === 0
        pure ()

prop_addAndGetTagsForPost :: TestDBConfig -> PropertyT IO ()
prop_addAndGetTagsForPost cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template <- forAllT $ blogPostInsertGen (User.Id 1)
    tagInsert1 <- forAllT blogTagInsertGen
    tagInsert2 <- forAllT blogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let post = template {UUT.bpiAuthorId = userId}
        postId <- unwrapInsert (UUT.insertBlogPost post)

        -- Insert tags (suffix to ensure unique names under shrinking)
        tagId1 <- unwrapInsert (BlogTags.insertTag tagInsert1 {BlogTags.btiName = BlogTags.btiName tagInsert1 <> "1"})
        tagId2 <- unwrapInsert (BlogTags.insertTag tagInsert2 {BlogTags.btiName = BlogTags.btiName tagInsert2 <> "2"})

        -- Add tags to post
        TRX.statement () (UUT.addTagToPost postId tagId1)
        TRX.statement () (UUT.addTagToPost postId tagId2)

        -- Get tags
        tags <- TRX.statement () (UUT.getTagsForPost postId)

        -- Adding same tag again should not error (ON CONFLICT DO NOTHING)
        TRX.statement () (UUT.addTagToPost postId tagId1)
        tagsAfterDupe <- TRX.statement () (UUT.getTagsForPost postId)

        TRX.condemn
        pure (tagInsert1, tagInsert2, tags, tagsAfterDupe)

      assert $ do
        (ti1, ti2, tags, tagsAfterDupe) <- assertRight result
        -- Should have 2 tags
        length tags === 2

        -- Should contain both tag names (with suffixes applied in transaction)
        map BlogTags.btmName tags =\\= [BlogTags.btiName ti1 <> "1", BlogTags.btiName ti2 <> "2"]

        -- Idempotent add: still 2 tags
        length tagsAfterDupe === 2
        pure ()

prop_removeTagFromPost :: TestDBConfig -> PropertyT IO ()
prop_removeTagFromPost cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template <- forAllT $ blogPostInsertGen (User.Id 1)
    tagInsert <- forAllT blogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let post = template {UUT.bpiAuthorId = userId}
        postId <- unwrapInsert (UUT.insertBlogPost post)

        tagId <- unwrapInsert (BlogTags.insertTag tagInsert)
        TRX.statement () (UUT.addTagToPost postId tagId)

        -- Remove the tag
        TRX.statement () (UUT.removeTagFromPost postId tagId)
        tags <- TRX.statement () (UUT.getTagsForPost postId)
        TRX.condemn
        pure tags

      assert $ do
        tags <- assertRight result
        length tags === 0
        pure ()

prop_getPostsByTag :: TestDBConfig -> PropertyT IO ()
prop_getPostsByTag cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ blogPostInsertGen (User.Id 1)
    template2 <- forAllT $ blogPostInsertGen (User.Id 1)
    template3 <- forAllT $ blogPostInsertGen (User.Id 1)
    tagInsert <- forAllT blogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let publishedPost = template1 {UUT.bpiAuthorId = userId, UUT.bpiStatus = Published, UUT.bpiSlug = UUT.bpiSlug template1 <> Slug "1"}
        let draftPost = template2 {UUT.bpiAuthorId = userId, UUT.bpiStatus = Draft, UUT.bpiSlug = UUT.bpiSlug template2 <> Slug "2"}
        let deletedPost = template3 {UUT.bpiAuthorId = userId, UUT.bpiStatus = Deleted, UUT.bpiSlug = UUT.bpiSlug template3 <> Slug "3"}

        publishedId <- unwrapInsert (UUT.insertBlogPost publishedPost)
        draftId <- unwrapInsert (UUT.insertBlogPost draftPost)
        deletedId <- unwrapInsert (UUT.insertBlogPost deletedPost)

        tagId <- unwrapInsert (BlogTags.insertTag tagInsert)

        -- Tag all posts
        TRX.statement () (UUT.addTagToPost publishedId tagId)
        TRX.statement () (UUT.addTagToPost draftId tagId)
        TRX.statement () (UUT.addTagToPost deletedId tagId)

        -- getPostsByTag should only return published posts
        posts <- TRX.statement () (UUT.getPostsByTag tagId (Limit 10) (Offset 0))
        TRX.condemn
        pure (publishedId, posts)

      assert $ do
        (publishedId, posts) <- assertRight result
        post <- assertSingleton posts
        UUT.bpmId post === publishedId
        UUT.bpmStatus post === Published
        pure ()

prop_getPostsByTag_limit :: TestDBConfig -> PropertyT IO ()
prop_getPostsByTag_limit cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ blogPostInsertGen (User.Id 1)
    template2 <- forAllT $ blogPostInsertGen (User.Id 1)
    template3 <- forAllT $ blogPostInsertGen (User.Id 1)
    tagInsert <- forAllT blogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let post1 = template1 {UUT.bpiAuthorId = userId, UUT.bpiStatus = Published, UUT.bpiSlug = UUT.bpiSlug template1 <> Slug "1"}
        let post2 = template2 {UUT.bpiAuthorId = userId, UUT.bpiStatus = Published, UUT.bpiSlug = UUT.bpiSlug template2 <> Slug "2"}
        let post3 = template3 {UUT.bpiAuthorId = userId, UUT.bpiStatus = Published, UUT.bpiSlug = UUT.bpiSlug template3 <> Slug "3"}

        id1 <- unwrapInsert (UUT.insertBlogPost post1)
        id2 <- unwrapInsert (UUT.insertBlogPost post2)
        id3 <- unwrapInsert (UUT.insertBlogPost post3)

        tagId <- unwrapInsert (BlogTags.insertTag tagInsert)
        TRX.statement () (UUT.addTagToPost id1 tagId)
        TRX.statement () (UUT.addTagToPost id2 tagId)
        TRX.statement () (UUT.addTagToPost id3 tagId)

        limited <- TRX.statement () (UUT.getPostsByTag tagId (Limit 2) (Offset 0))
        TRX.condemn
        pure limited

      assert $ do
        limited <- assertRight result
        length limited === 2
        pure ()

prop_getPostsByTag_offset :: TestDBConfig -> PropertyT IO ()
prop_getPostsByTag_offset cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    template1 <- forAllT $ blogPostInsertGen (User.Id 1)
    template2 <- forAllT $ blogPostInsertGen (User.Id 1)
    template3 <- forAllT $ blogPostInsertGen (User.Id 1)
    tagInsert <- forAllT blogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata

        let post1 = template1 {UUT.bpiAuthorId = userId, UUT.bpiStatus = Published, UUT.bpiSlug = UUT.bpiSlug template1 <> Slug "1"}
        let post2 = template2 {UUT.bpiAuthorId = userId, UUT.bpiStatus = Published, UUT.bpiSlug = UUT.bpiSlug template2 <> Slug "2"}
        let post3 = template3 {UUT.bpiAuthorId = userId, UUT.bpiStatus = Published, UUT.bpiSlug = UUT.bpiSlug template3 <> Slug "3"}

        id1 <- unwrapInsert (UUT.insertBlogPost post1)
        id2 <- unwrapInsert (UUT.insertBlogPost post2)
        id3 <- unwrapInsert (UUT.insertBlogPost post3)

        tagId <- unwrapInsert (BlogTags.insertTag tagInsert)
        TRX.statement () (UUT.addTagToPost id1 tagId)
        TRX.statement () (UUT.addTagToPost id2 tagId)
        TRX.statement () (UUT.addTagToPost id3 tagId)

        allPosts <- TRX.statement () (UUT.getPostsByTag tagId (Limit 10) (Offset 0))
        offset1 <- TRX.statement () (UUT.getPostsByTag tagId (Limit 10) (Offset 1))
        offset2 <- TRX.statement () (UUT.getPostsByTag tagId (Limit 10) (Offset 2))
        offset3 <- TRX.statement () (UUT.getPostsByTag tagId (Limit 10) (Offset 3))
        TRX.condemn
        pure (allPosts, offset1, offset2, offset3)

      assert $ do
        (allPosts, offset1, offset2, offset3) <- assertRight result
        length allPosts === 3
        length offset1 === 2
        length offset2 === 1
        length offset3 === 0
        pure ()
