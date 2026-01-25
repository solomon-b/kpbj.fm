module Test.Gen.Tables.SitePages where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Effects.Database.Tables.SitePages qualified as SitePages
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.Text (genSlug, genText)

--------------------------------------------------------------------------------

-- | Generate a site page insert.
sitePageInsertGen :: (MonadGen m) => m SitePages.Insert
sitePageInsertGen = do
  spiSlug <- genSlug
  spiTitle <- genText
  spiContent <- genMarkdownContent
  pure SitePages.Insert {..}

-- | Generate a site page update.
sitePageUpdateGen :: (MonadGen m) => m SitePages.Update
sitePageUpdateGen = do
  spuTitle <- genText
  spuContent <- genMarkdownContent
  pure SitePages.Update {..}

-- | Generate markdown-like content for site pages.
genMarkdownContent :: (MonadGen m) => m Text
genMarkdownContent = do
  heading <- genText
  paragraph1 <- genText
  paragraph2 <- genText
  pure $ "# " <> heading <> "\n\n" <> paragraph1 <> "\n\n" <> paragraph2

-- | Generate an edit summary.
genEditSummary :: (MonadGen m) => m (Maybe Text)
genEditSummary = Gen.maybe $ Gen.text (Range.linear 5 100) Gen.alpha
