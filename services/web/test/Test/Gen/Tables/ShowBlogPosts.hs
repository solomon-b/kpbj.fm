module Test.Gen.Tables.ShowBlogPosts where

--------------------------------------------------------------------------------

import Domain.Types.PostStatus (BlogPostStatus (..))
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Test.Gen.DomainTypes (genSlug)
import Test.Gen.Text (genText)

--------------------------------------------------------------------------------

genBlogPostStatus :: (MonadGen m) => m BlogPostStatus
genBlogPostStatus = Gen.enumBounded

showBlogPostInsertGen :: (MonadGen m) => Shows.Id -> User.Id -> m ShowBlogPosts.Insert
showBlogPostInsertGen showId userId = do
  sbpiTitle <- genText
  sbpiSlug <- genSlug
  sbpiContent <- genText
  sbpiExcerpt <- Gen.maybe genText
  let sbpiId = showId
  let sbpiAuthorId = userId
  sbpiStatus <- genBlogPostStatus
  pure ShowBlogPosts.Insert {..}
