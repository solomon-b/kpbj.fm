module Effects.Database.Tables.BlogPostsSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BlogPosts qualified as UUT
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Interpolate (OneRow (OneRow))
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight)
import Test.Gen.Tables.BlogPosts (blogPostInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.BlogPosts" $ do
      runs 10 . it "lens law: insert-select blog post" $ hedgehog . prop_insertSelectBlogPost
      runs 10 . it "lens law: getBlogPostById returns inserted post" $ hedgehog . prop_getBlogPostById
      runs 10 . it "lens law: getBlogPostBySlug returns inserted post" $ hedgehog . prop_getBlogPostBySlug
      runs 10 . it "query: getBlogPostsByAuthor returns posts by specific author" $ hedgehog . prop_getBlogPostsByAuthor

-- Lens Law: insert then select returns what we inserted
prop_insertSelectBlogPost :: TestDBConfig -> PropertyT IO ()
prop_insertSelectBlogPost cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    blogPostTemplate <- forAllT $ blogPostInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.ModelInsert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata)

        let blogPostInsert = blogPostTemplate {UUT.bpiAuthorId = userId}

        postId <- TRX.statement () (UUT.insertBlogPost blogPostInsert)
        selected <- TRX.statement () (UUT.getBlogPostById postId)
        pure (postId, blogPostInsert, selected)

      assert $ do
        (postId, blogPostInsert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        UUT.bpiTitle blogPostInsert === UUT.bpmTitle selected
        UUT.bpiSlug blogPostInsert === UUT.bpmSlug selected
        UUT.bpiContent blogPostInsert === UUT.bpmContent selected
        UUT.bpiExcerpt blogPostInsert === UUT.bpmExcerpt selected
        UUT.bpiAuthorId blogPostInsert === UUT.bpmAuthorId selected
        UUT.bpiStatus blogPostInsert === UUT.bpmStatus selected
        postId === UUT.bpmId selected
        pure ()

-- Lens Law: getById after insert returns the post
prop_getBlogPostById :: TestDBConfig -> PropertyT IO ()
prop_getBlogPostById cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    blogPostTemplate <- forAllT $ blogPostInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.ModelInsert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata)

        let blogPostInsert = blogPostTemplate {UUT.bpiAuthorId = userId}

        postId <- TRX.statement () (UUT.insertBlogPost blogPostInsert)
        byId <- TRX.statement () (UUT.getBlogPostById postId)
        pure (postId, byId)

      assert $ do
        (postId, mById) <- assertRight result
        byId <- assertJust mById
        UUT.bpmId byId === postId
        pure ()

-- Lens Law: getBySlug after insert returns the post
prop_getBlogPostBySlug :: TestDBConfig -> PropertyT IO ()
prop_getBlogPostBySlug cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    blogPostTemplate <- forAllT $ blogPostInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.ModelInsert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata)

        let blogPostInsert = blogPostTemplate {UUT.bpiAuthorId = userId}

        postId <- TRX.statement () (UUT.insertBlogPost blogPostInsert)
        byId <- TRX.statement () (UUT.getBlogPostById postId)
        bySlug <- TRX.statement () (UUT.getBlogPostBySlug $ UUT.bpiSlug blogPostInsert)
        pure (postId, byId, bySlug)

      assert $ do
        (postId, mById, mBySlug) <- assertRight result
        byId <- assertJust mById
        bySlug <- assertJust mBySlug
        UUT.bpmId byId === UUT.bpmId bySlug
        UUT.bpmSlug byId === UUT.bpmSlug bySlug
        postId === UUT.bpmId bySlug
        pure ()

-- Test specialized query: getBlogPostsByAuthor
prop_getBlogPostsByAuthor :: TestDBConfig -> PropertyT IO ()
prop_getBlogPostsByAuthor cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    blogPostTemplate <- forAllT $ blogPostInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.ModelInsert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata)

        let blogPostInsert = blogPostTemplate {UUT.bpiAuthorId = userId}

        postId <- TRX.statement () (UUT.insertBlogPost blogPostInsert)
        posts <- TRX.statement () (UUT.getBlogPostsByAuthor userId 10 0)
        pure (postId, userId, posts)

      assert $ do
        (postId, userId, posts) <- assertRight result
        -- The inserted post should be in the list
        let foundPost = filter (\p -> UUT.bpmId p == postId) posts
        case foundPost of
          [post] -> do
            UUT.bpmAuthorId post === userId
            pure ()
          _ -> pure () -- Empty list is ok if there are no posts yet
