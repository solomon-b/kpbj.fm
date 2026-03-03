module Test.Gen.Tables.ShowTags where

--------------------------------------------------------------------------------

import Effects.Database.Tables.ShowTags qualified as ShowTags
import Hedgehog (MonadGen (..))
import Test.Gen.Text (genShortText)

--------------------------------------------------------------------------------

showTagInsertGen :: (MonadGen m) => m ShowTags.Insert
showTagInsertGen = do
  stiName <- genShortText
  pure ShowTags.Insert {..}
