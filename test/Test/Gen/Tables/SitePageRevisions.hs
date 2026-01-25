module Test.Gen.Tables.SitePageRevisions where

--------------------------------------------------------------------------------

import Effects.Database.Tables.SitePageRevisions qualified as SitePageRevisions
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Database.Tables.User qualified as User
import Hedgehog (MonadGen (..))
import Test.Gen.Tables.SitePages (genEditSummary, genMarkdownContent)

--------------------------------------------------------------------------------

-- | Generate a site page revision insert.
sitePageRevisionInsertGen :: (MonadGen m) => SitePages.Id -> User.Id -> m SitePageRevisions.Insert
sitePageRevisionInsertGen pageId userId = do
  spriContent <- genMarkdownContent
  spriEditSummary <- genEditSummary
  let spriPageId = pageId
      spriCreatedBy = userId
  pure SitePageRevisions.Insert {..}
