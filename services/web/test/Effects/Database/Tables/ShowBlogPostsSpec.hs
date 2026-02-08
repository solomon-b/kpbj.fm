{-# LANGUAGE OverloadedRecordDot #-}

module Effects.Database.Tables.ShowBlogPostsSpec where

--------------------------------------------------------------------------------

import Control.Monad (void)
import Data.Either (isLeft)
import Data.Text.Display (display)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ShowBlogPosts qualified as UUT
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, assertSingleton, (->-), (<==), (=\\=))
import Test.Gen.Tables.ShowBlogPosts (showBlogPostInsertGen)
import Test.Gen.Tables.ShowBlogTags (showBlogTagInsertGen)
import Test.Gen.Tables.Shows (showInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.ShowBlogPosts" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: inserted fields preserved on select" $
          hedgehog . prop_insertSelect
        runs 10 . it "update-select: updated fields overwrite original on select" $
          hedgehog . prop_updateSelect
        runs 10 . it "update-update: second update fully overwrites first" $
          hedgehog . prop_updateUpdate

      describe "Queries" $ do
        describe "getShowBlogPosts" $ do
          runs 10 . it "returns all posts for show regardless of status" $ hedgehog . prop_getShowBlogPosts
        describe "getPublishedShowBlogPosts" $ do
          runs 10 . it "filters by Published status" $ hedgehog . prop_getPublishedShowBlogPosts
          runs 10 . it "respects Limit" $ hedgehog . prop_getPublishedShowBlogPosts_limit
          runs 10 . it "respects Offset" $ hedgehog . prop_getPublishedShowBlogPosts_offset

      describe "Mutations" $ do
        runs 10 . it "deleteShowBlogPost: removes post and is idempotent" $
          hedgehog . prop_deleteShowBlogPost
        runs 10 . it "updateShowBlogPost: manages published_at transitions" $
          hedgehog . prop_updateBlogPost_publishedAt

      describe "Constraints" $ do
        runs 10 . it "rejects duplicate slug per show on insert" $ hedgehog . prop_insertDuplicateSlugPerShow

      describe "Tag Operations" $ do
        runs 10 . it "addTagToShowBlogPost and getTagsForShowBlogPost" $ hedgehog . prop_addAndGetTagsForPost
        runs 10 . it "removeTagFromShowBlogPost removes tag" $ hedgehog . prop_removeTagFromPost
        runs 10 . it "deleteShowBlogPost cascades to tags" $ hedgehog . prop_deleteBlogPost_cascade

      describe "Show-Slug Queries" $ do
        runs 10 . it "getPublishedShowBlogPostsBySlug: returns published posts by show slug" $ hedgehog . prop_getPublishedShowBlogPostsBySlug
        runs 10 . it "getPublishedShowBlogPostsByShowAndTag: filters by show and tag" $ hedgehog . prop_getPublishedShowBlogPostsByShowAndTag
        runs 10 . it "countPublishedShowBlogPosts: counts published posts" $ hedgehog . prop_countPublishedShowBlogPosts
        runs 10 . it "countPublishedShowBlogPostsByTag: counts by tag" $ hedgehog . prop_countPublishedShowBlogPostsByTag
        runs 10 . it "getTagsForShow: returns distinct tags from published posts" $ hedgehog . prop_getTagsForShow

--------------------------------------------------------------------------------
-- Helpers

-- | Set up a show and return its ID.
insertTestShow :: Shows.Insert -> TRX.Transaction Shows.Id
insertTestShow showInsert = unwrapInsert (Shows.insertShow showInsert)

-- | Assert all user-provided fields in an Insert match the corresponding Model fields.
assertInsertFieldsMatch :: UUT.Insert -> UUT.Model -> PropertyT IO ()
assertInsertFieldsMatch insert model = do
  UUT.sbpiTitle insert === model.title
  UUT.sbpiSlug insert === model.slug
  UUT.sbpiContent insert === model.content
  UUT.sbpiExcerpt insert === model.excerpt
  UUT.sbpiAuthorId insert === model.authorId
  UUT.sbpiStatus insert === model.status

--------------------------------------------------------------------------------
-- Lens Laws

-- | Insert-Select: insert then select returns what we inserted.
prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    postTemplate <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let postInsert = postTemplate {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId}

        postId <- unwrapInsert (UUT.insertShowBlogPost postInsert)
        selected <- TRX.statement () (UUT.getShowBlogPostById postId)
        TRX.condemn
        pure (postId, postInsert, selected)

      assert $ do
        (postId, postInsert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch postInsert selected
        selected.id === postId
        postId ->- UUT.Id 0
        case UUT.sbpiStatus postInsert of
          Published -> void $ assertJust selected.publishedAt
          _ -> selected.publishedAt === Nothing
        pure ()

-- | Update-Select: update then select returns updated values.
prop_updateSelect :: TestDBConfig -> PropertyT IO ()
prop_updateSelect cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    originalTemplate <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    updateTemplate <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let original = originalTemplate {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Draft}
        postId <- unwrapInsert (UUT.insertShowBlogPost original)

        let updated = updateTemplate {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Draft}
        updateResult <- TRX.statement () (UUT.updateShowBlogPost postId updated)

        selected <- TRX.statement () (UUT.getShowBlogPostById postId)
        TRX.condemn
        pure (postId, updated, updateResult, selected)

      assert $ do
        (postId, updated, updateResult, mSelected) <- assertRight result
        updatedId <- assertJust updateResult
        updatedId === postId

        selected <- assertJust mSelected
        assertInsertFieldsMatch updated selected
        selected.id === postId
        selected.publishedAt === Nothing
        pure ()

-- | Update-Update: second update fully overwrites first.
prop_updateUpdate :: TestDBConfig -> PropertyT IO ()
prop_updateUpdate cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    originalTemplate <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    updateATemplate <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    updateBTemplate <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let original = originalTemplate {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Draft}
        postId <- unwrapInsert (UUT.insertShowBlogPost original)

        let updateA = updateATemplate {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Draft}
        _ <- TRX.statement () (UUT.updateShowBlogPost postId updateA)

        let updateB = updateBTemplate {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Draft}
        _ <- TRX.statement () (UUT.updateShowBlogPost postId updateB)

        selected <- TRX.statement () (UUT.getShowBlogPostById postId)
        TRX.condemn
        pure (postId, updateB, selected)

      assert $ do
        (postId, updateB, mSelected) <- assertRight result
        selected <- assertJust mSelected
        assertInsertFieldsMatch updateB selected
        selected.id === postId
        selected.publishedAt === Nothing
        pure ()

--------------------------------------------------------------------------------
-- Query tests

-- | getShowBlogPosts: returns all posts for a show regardless of status.
prop_getShowBlogPosts :: TestDBConfig -> PropertyT IO ()
prop_getShowBlogPosts cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template1 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    template2 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    template3 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let post1 = template1 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Draft, UUT.sbpiSlug = UUT.sbpiSlug template1 <> Slug "1"}
        let post2 = template2 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template2 <> Slug "2"}
        let post3 = template3 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Deleted, UUT.sbpiSlug = UUT.sbpiSlug template3 <> Slug "3"}

        id1 <- unwrapInsert (UUT.insertShowBlogPost post1)
        id2 <- unwrapInsert (UUT.insertShowBlogPost post2)
        id3 <- unwrapInsert (UUT.insertShowBlogPost post3)

        allPosts <- TRX.statement () (UUT.getShowBlogPosts showId (Limit 10) (Offset 0))
        TRX.condemn
        pure (id1, id2, id3, allPosts)

      assert $ do
        (id1, id2, id3, allPosts) <- assertRight result
        length allPosts === 3
        let returnedIds = map (.id) allPosts
        returnedIds =\\= [id1, id2, id3]
        pure ()

-- | getPublishedShowBlogPosts: filters by Published status.
prop_getPublishedShowBlogPosts :: TestDBConfig -> PropertyT IO ()
prop_getPublishedShowBlogPosts cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template1 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    template2 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let publishedPost = template1 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template1 <> Slug "1"}
        let draftPost = template2 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Draft, UUT.sbpiSlug = UUT.sbpiSlug template2 <> Slug "2"}

        publishedId <- unwrapInsert (UUT.insertShowBlogPost publishedPost)
        _draftId <- unwrapInsert (UUT.insertShowBlogPost draftPost)

        published <- TRX.statement () (UUT.getPublishedShowBlogPosts showId (Limit 10) (Offset 0))
        TRX.condemn
        pure (publishedId, published)

      assert $ do
        (publishedId, published) <- assertRight result
        post <- assertSingleton published
        post.id === publishedId
        post.status === Published
        pure ()

-- | getPublishedShowBlogPosts: respects Limit.
prop_getPublishedShowBlogPosts_limit :: TestDBConfig -> PropertyT IO ()
prop_getPublishedShowBlogPosts_limit cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template1 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    template2 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    template3 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let post1 = template1 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template1 <> Slug "1"}
        let post2 = template2 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template2 <> Slug "2"}
        let post3 = template3 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template3 <> Slug "3"}

        _ <- unwrapInsert (UUT.insertShowBlogPost post1)
        _ <- unwrapInsert (UUT.insertShowBlogPost post2)
        _ <- unwrapInsert (UUT.insertShowBlogPost post3)

        limited <- TRX.statement () (UUT.getPublishedShowBlogPosts showId (Limit 2) (Offset 0))
        TRX.condemn
        pure limited

      assert $ do
        limited <- assertRight result
        length limited === 2
        pure ()

-- | getPublishedShowBlogPosts: respects Offset.
prop_getPublishedShowBlogPosts_offset :: TestDBConfig -> PropertyT IO ()
prop_getPublishedShowBlogPosts_offset cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template1 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    template2 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    template3 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let post1 = template1 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template1 <> Slug "1"}
        let post2 = template2 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template2 <> Slug "2"}
        let post3 = template3 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template3 <> Slug "3"}

        _ <- unwrapInsert (UUT.insertShowBlogPost post1)
        _ <- unwrapInsert (UUT.insertShowBlogPost post2)
        _ <- unwrapInsert (UUT.insertShowBlogPost post3)

        allPosts <- TRX.statement () (UUT.getPublishedShowBlogPosts showId (Limit 10) (Offset 0))
        offset1 <- TRX.statement () (UUT.getPublishedShowBlogPosts showId (Limit 10) (Offset 1))
        offset2 <- TRX.statement () (UUT.getPublishedShowBlogPosts showId (Limit 10) (Offset 2))
        offset3 <- TRX.statement () (UUT.getPublishedShowBlogPosts showId (Limit 10) (Offset 3))
        TRX.condemn
        pure (allPosts, offset1, offset2, offset3)

      assert $ do
        (allPosts, offset1, offset2, offset3) <- assertRight result
        length allPosts === 3
        length offset1 === 2
        length offset2 === 1
        length offset3 === 0
        pure ()

--------------------------------------------------------------------------------
-- Mutation tests

-- | deleteShowBlogPost: removes post, second delete returns Nothing.
prop_deleteShowBlogPost :: TestDBConfig -> PropertyT IO ()
prop_deleteShowBlogPost cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let post = template {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId}
        postId <- unwrapInsert (UUT.insertShowBlogPost post)

        deleteResult <- TRX.statement () (UUT.deleteShowBlogPost postId)
        afterDelete <- TRX.statement () (UUT.getShowBlogPostById postId)
        deleteAgain <- TRX.statement () (UUT.deleteShowBlogPost postId)

        TRX.condemn
        pure (postId, deleteResult, afterDelete, deleteAgain)

      assert $ do
        (postId, deleteResult, afterDelete, deleteAgain) <- assertRight result
        deletedId <- assertJust deleteResult
        deletedId === postId
        assertNothing afterDelete
        assertNothing deleteAgain
        pure ()

-- | updateShowBlogPost: manages published_at transitions.
prop_updateBlogPost_publishedAt :: TestDBConfig -> PropertyT IO ()
prop_updateBlogPost_publishedAt cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    updateTemplate <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        -- Insert as Draft
        let draftPost = template {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Draft}
        postId <- unwrapInsert (UUT.insertShowBlogPost draftPost)

        -- Transition Draft -> Published: published_at should be set
        let publishUpdate = updateTemplate {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published}
        _ <- TRX.statement () (UUT.updateShowBlogPost postId publishUpdate)
        afterPublish <- TRX.statement () (UUT.getShowBlogPostById postId)

        -- Update Published -> Published: published_at should remain the same
        let stayPublished = updateTemplate {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published}
        _ <- TRX.statement () (UUT.updateShowBlogPost postId stayPublished)
        afterStayPublished <- TRX.statement () (UUT.getShowBlogPostById postId)

        -- Transition Published -> Draft: published_at should be cleared
        let backToDraft = updateTemplate {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Draft}
        _ <- TRX.statement () (UUT.updateShowBlogPost postId backToDraft)
        afterBackToDraft <- TRX.statement () (UUT.getShowBlogPostById postId)

        TRX.condemn
        pure (afterPublish, afterStayPublished, afterBackToDraft)

      assert $ do
        (mAfterPublish, mAfterStayPublished, mAfterBackToDraft) <- assertRight result

        afterPublish <- assertJust mAfterPublish
        publishedAt1 <- assertJust afterPublish.publishedAt

        afterStayPublished <- assertJust mAfterStayPublished
        publishedAt2 <- assertJust afterStayPublished.publishedAt
        publishedAt1 === publishedAt2

        afterBackToDraft <- assertJust mAfterBackToDraft
        afterBackToDraft.publishedAt === Nothing
        pure ()

--------------------------------------------------------------------------------
-- Constraint tests

-- | Rejects duplicate slug per show on insert.
prop_insertDuplicateSlugPerShow :: TestDBConfig -> PropertyT IO ()
prop_insertDuplicateSlugPerShow cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template1 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    template2 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let post1 = template1 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId}
        _ <- unwrapInsert (UUT.insertShowBlogPost post1)

        -- Insert with same slug
        let post2 = template2 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiSlug = UUT.sbpiSlug post1}
        _ <- unwrapInsert (UUT.insertShowBlogPost post2)
        TRX.condemn
        pure ()

      assert $ do
        result <== isLeft
        pure ()

--------------------------------------------------------------------------------
-- Tag tests

-- | addTagToShowBlogPost and getTagsForShowBlogPost.
prop_addAndGetTagsForPost :: TestDBConfig -> PropertyT IO ()
prop_addAndGetTagsForPost cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    tagInsert1 <- forAllT showBlogTagInsertGen
    tagInsert2 <- forAllT showBlogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let post = template {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId}
        postId <- unwrapInsert (UUT.insertShowBlogPost post)

        -- Insert tags (suffix to ensure unique names)
        tagId1 <- unwrapInsert (ShowBlogTags.insertShowBlogTag tagInsert1 {ShowBlogTags.sbtiName = ShowBlogTags.sbtiName tagInsert1 <> "1"})
        tagId2 <- unwrapInsert (ShowBlogTags.insertShowBlogTag tagInsert2 {ShowBlogTags.sbtiName = ShowBlogTags.sbtiName tagInsert2 <> "2"})

        -- Add tags to post
        TRX.statement () (UUT.addTagToShowBlogPost postId tagId1)
        TRX.statement () (UUT.addTagToShowBlogPost postId tagId2)

        -- Get tags
        tags <- TRX.statement () (UUT.getTagsForShowBlogPost postId)

        -- Idempotent add
        TRX.statement () (UUT.addTagToShowBlogPost postId tagId1)
        tagsAfterDupe <- TRX.statement () (UUT.getTagsForShowBlogPost postId)

        TRX.condemn
        pure (tagInsert1, tagInsert2, tags, tagsAfterDupe)

      assert $ do
        (ti1, ti2, tags, tagsAfterDupe) <- assertRight result
        length tags === 2
        map ShowBlogTags.sbtmName tags =\\= [ShowBlogTags.sbtiName ti1 <> "1", ShowBlogTags.sbtiName ti2 <> "2"]
        length tagsAfterDupe === 2
        pure ()

-- | removeTagFromShowBlogPost removes tag.
prop_removeTagFromPost :: TestDBConfig -> PropertyT IO ()
prop_removeTagFromPost cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    tagInsert <- forAllT showBlogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let post = template {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId}
        postId <- unwrapInsert (UUT.insertShowBlogPost post)

        tagId <- unwrapInsert (ShowBlogTags.insertShowBlogTag tagInsert)
        TRX.statement () (UUT.addTagToShowBlogPost postId tagId)

        -- Remove the tag
        TRX.statement () (UUT.removeTagFromShowBlogPost postId tagId)
        tags <- TRX.statement () (UUT.getTagsForShowBlogPost postId)
        TRX.condemn
        pure tags

      assert $ do
        tags <- assertRight result
        length tags === 0
        pure ()

-- | deleteShowBlogPost cascades to show_blog_post_tags.
prop_deleteBlogPost_cascade :: TestDBConfig -> PropertyT IO ()
prop_deleteBlogPost_cascade cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    tagInsert <- forAllT showBlogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let post = template {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId}
        postId <- unwrapInsert (UUT.insertShowBlogPost post)

        tagId <- unwrapInsert (ShowBlogTags.insertShowBlogTag tagInsert)
        TRX.statement () (UUT.addTagToShowBlogPost postId tagId)

        -- Verify tag is attached
        tagsBefore <- TRX.statement () (UUT.getTagsForShowBlogPost postId)

        -- Delete the post
        _ <- TRX.statement () (UUT.deleteShowBlogPost postId)

        -- Junction rows should be gone (CASCADE)
        tagsAfter <- TRX.statement () (UUT.getTagsForShowBlogPost postId)

        TRX.condemn
        pure (tagsBefore, tagsAfter)

      assert $ do
        (tagsBefore, tagsAfter) <- assertRight result
        length tagsBefore === 1
        length tagsAfter === 0
        pure ()

--------------------------------------------------------------------------------
-- Show-Slug Query tests

-- | getPublishedShowBlogPostsBySlug: returns published posts by show slug.
prop_getPublishedShowBlogPostsBySlug :: TestDBConfig -> PropertyT IO ()
prop_getPublishedShowBlogPostsBySlug cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template1 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    template2 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let publishedPost = template1 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template1 <> Slug "1"}
        let draftPost = template2 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Draft, UUT.sbpiSlug = UUT.sbpiSlug template2 <> Slug "2"}

        publishedId <- unwrapInsert (UUT.insertShowBlogPost publishedPost)
        _draftId <- unwrapInsert (UUT.insertShowBlogPost draftPost)

        let showSlug = Shows.siSlug showInsert
        posts <- TRX.statement () (UUT.getPublishedShowBlogPostsBySlug (display showSlug) (Limit 10) (Offset 0))

        -- Limit/Offset respected
        limited <- TRX.statement () (UUT.getPublishedShowBlogPostsBySlug (display showSlug) (Limit 0) (Offset 0))

        TRX.condemn
        pure (publishedId, posts, limited)

      assert $ do
        (publishedId, posts, limited) <- assertRight result
        -- Only published post returned
        post <- assertSingleton posts
        post.id === publishedId
        post.status === Published
        -- Limit 0 returns nothing
        length limited === 0
        pure ()

-- | getPublishedShowBlogPostsByShowAndTag: filters by show and tag.
prop_getPublishedShowBlogPostsByShowAndTag :: TestDBConfig -> PropertyT IO ()
prop_getPublishedShowBlogPostsByShowAndTag cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template1 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    template2 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    tagInsert <- forAllT showBlogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let taggedPost = template1 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template1 <> Slug "1"}
        let untaggedPost = template2 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template2 <> Slug "2"}

        taggedId <- unwrapInsert (UUT.insertShowBlogPost taggedPost)
        _untaggedId <- unwrapInsert (UUT.insertShowBlogPost untaggedPost)

        -- Tag only the first post
        tagId <- unwrapInsert (ShowBlogTags.insertShowBlogTag tagInsert)
        TRX.statement () (UUT.addTagToShowBlogPost taggedId tagId)

        posts <- TRX.statement () (UUT.getPublishedShowBlogPostsByShowAndTag showId tagId (Limit 10) (Offset 0))
        TRX.condemn
        pure (taggedId, posts)

      assert $ do
        (taggedId, posts) <- assertRight result
        post <- assertSingleton posts
        post.id === taggedId
        pure ()

-- | countPublishedShowBlogPosts: counts published posts.
prop_countPublishedShowBlogPosts :: TestDBConfig -> PropertyT IO ()
prop_countPublishedShowBlogPosts cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template1 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    template2 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    template3 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let pub1 = template1 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template1 <> Slug "1"}
        let pub2 = template2 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template2 <> Slug "2"}
        let draft = template3 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Draft, UUT.sbpiSlug = UUT.sbpiSlug template3 <> Slug "3"}

        _ <- unwrapInsert (UUT.insertShowBlogPost pub1)
        _ <- unwrapInsert (UUT.insertShowBlogPost pub2)
        _ <- unwrapInsert (UUT.insertShowBlogPost draft)

        count <- TRX.statement () (UUT.countPublishedShowBlogPosts showId)
        TRX.condemn
        pure count

      assert $ do
        count <- assertRight result
        count === 2

-- | countPublishedShowBlogPostsByTag: counts published posts filtered by tag.
prop_countPublishedShowBlogPostsByTag :: TestDBConfig -> PropertyT IO ()
prop_countPublishedShowBlogPostsByTag cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template1 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    template2 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    tagInsert <- forAllT showBlogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        let taggedPost = template1 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template1 <> Slug "1"}
        let untaggedPost = template2 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template2 <> Slug "2"}

        taggedId <- unwrapInsert (UUT.insertShowBlogPost taggedPost)
        _ <- unwrapInsert (UUT.insertShowBlogPost untaggedPost)

        tagId <- unwrapInsert (ShowBlogTags.insertShowBlogTag tagInsert)
        TRX.statement () (UUT.addTagToShowBlogPost taggedId tagId)

        count <- TRX.statement () (UUT.countPublishedShowBlogPostsByTag showId tagId)
        TRX.condemn
        pure count

      assert $ do
        count <- assertRight result
        count === 1

-- | getTagsForShow: returns distinct tags from published posts.
prop_getTagsForShow :: TestDBConfig -> PropertyT IO ()
prop_getTagsForShow cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template1 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    template2 <- forAllT $ showBlogPostInsertGen (Shows.Id 1) (User.Id 1)
    tagInsert1 <- forAllT showBlogTagInsertGen
    tagInsert2 <- forAllT showBlogTagInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        showId <- insertTestShow showInsert

        -- Published post with tag
        let publishedPost = template1 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Published, UUT.sbpiSlug = UUT.sbpiSlug template1 <> Slug "1"}
        -- Draft post with a different tag (should not appear)
        let draftPost = template2 {UUT.sbpiId = showId, UUT.sbpiAuthorId = userId, UUT.sbpiStatus = Draft, UUT.sbpiSlug = UUT.sbpiSlug template2 <> Slug "2"}

        publishedId <- unwrapInsert (UUT.insertShowBlogPost publishedPost)
        draftId <- unwrapInsert (UUT.insertShowBlogPost draftPost)

        tagId1 <- unwrapInsert (ShowBlogTags.insertShowBlogTag tagInsert1 {ShowBlogTags.sbtiName = ShowBlogTags.sbtiName tagInsert1 <> "1"})
        tagId2 <- unwrapInsert (ShowBlogTags.insertShowBlogTag tagInsert2 {ShowBlogTags.sbtiName = ShowBlogTags.sbtiName tagInsert2 <> "2"})

        -- Tag published post with tag1
        TRX.statement () (UUT.addTagToShowBlogPost publishedId tagId1)
        -- Tag draft post with tag2
        TRX.statement () (UUT.addTagToShowBlogPost draftId tagId2)

        tags <- TRX.statement () (UUT.getTagsForShow showId)
        TRX.condemn
        pure (tagId1, tags)

      assert $ do
        (tagId1, tags) <- assertRight result
        -- Only tag from published post should appear
        tag <- assertSingleton tags
        ShowBlogTags.sbtmId tag === tagId1
        pure ()
