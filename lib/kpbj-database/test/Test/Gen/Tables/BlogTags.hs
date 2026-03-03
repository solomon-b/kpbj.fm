module Test.Gen.Tables.BlogTags where

--------------------------------------------------------------------------------

import Effects.Database.Tables.BlogTags qualified as BlogTags
import Hedgehog (MonadGen (..))
import Test.Gen.Text (genShortText)

--------------------------------------------------------------------------------

blogTagInsertGen :: (MonadGen m) => m BlogTags.Insert
blogTagInsertGen = do
  btiName <- genShortText
  pure BlogTags.Insert {..}
