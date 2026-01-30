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

-- Lens Law: insert then select returns what we inserted
prop_insertSelectBlogPost :: TestDBConfig -> PropertyT IO ()
prop_insertSelectBlogPost cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    blogPostTemplate <- forAllT $ blogPostInsertGen (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (OneRow userId) <- TRX.statement () $ User.insertUser $ User.ModelInsert (UserMetadata.uwmiEmail userWithMetadata) (UserMetadata.uwmiPassword userWithMetadata)
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata) (UserMetadata.uwmiColorScheme userWithMetadata) (UserMetadata.uwmiTheme userWithMetadata)

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
        _ <- TRX.statement () $ UserMetadata.insertUserMetadata $ UserMetadata.Insert userId (UserMetadata.uwmiDisplayName userWithMetadata) (UserMetadata.uwmiFullName userWithMetadata) (UserMetadata.uwmiAvatarUrl userWithMetadata) (UserMetadata.uwmiUserRole userWithMetadata) (UserMetadata.uwmiColorScheme userWithMetadata) (UserMetadata.uwmiTheme userWithMetadata)

        let blogPostInsert = blogPostTemplate {UUT.bpiAuthorId = userId}

        postId <- TRX.statement () (UUT.insertBlogPost blogPostInsert)
        byId <- TRX.statement () (UUT.getBlogPostById postId)
        pure (postId, byId)

      assert $ do
        (postId, mById) <- assertRight result
        byId <- assertJust mById
        UUT.bpmId byId === postId
        pure ()
