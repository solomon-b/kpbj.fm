module Test.Gen.Tables.ShowBlogTags where

--------------------------------------------------------------------------------

import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Hedgehog (MonadGen (..))
import Test.Gen.Text (genShortText)

--------------------------------------------------------------------------------

showBlogTagInsertGen :: (MonadGen m) => m ShowBlogTags.Insert
showBlogTagInsertGen = do
  sbtiName <- genShortText
  pure ShowBlogTags.Insert {..}
