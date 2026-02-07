module Test.Gen.Tables.PlaybackHistory where

--------------------------------------------------------------------------------

import Effects.Database.Tables.PlaybackHistory qualified as PlaybackHistory
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Test.Gen.Text (genText, genUrl)
import Test.Gen.Time (genUTCTime)

--------------------------------------------------------------------------------

playbackInsertGen :: (MonadGen m) => m PlaybackHistory.Insert
playbackInsertGen = do
  piTitle <- genText
  piArtist <- Gen.maybe genText
  piSourceType <- Gen.element ["episode", "ephemeral"] -- Free-form Text in schema; these are the known valid values
  piSourceUrl <- genUrl
  piStartedAt <- genUTCTime
  pure PlaybackHistory.Insert {..}
