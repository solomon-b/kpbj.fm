module Test.Gen.Tables.BlogPosts where

--------------------------------------------------------------------------------

import Domain.Types.PostStatus (BlogPostStatus (..))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.User qualified as User
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Test.Gen.DomainTypes (genSlug)
import Test.Gen.Text (genText)

--------------------------------------------------------------------------------

genBlogPostStatus :: (MonadGen m) => m BlogPostStatus
genBlogPostStatus = Gen.enumBounded

blogPostInsertGen :: (MonadGen m) => User.Id -> m BlogPosts.Insert
blogPostInsertGen userId = do
  bpiTitle <- genText
  bpiSlug <- genSlug
  bpiContent <- genText
  bpiExcerpt <- Gen.maybe genText
  bpiAuthorId <- pure userId
  bpiStatus <- genBlogPostStatus
  pure BlogPosts.Insert {..}
