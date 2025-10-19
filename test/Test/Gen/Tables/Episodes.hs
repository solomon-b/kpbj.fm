module Test.Gen.Tables.Episodes where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.DomainTypes (genSlug)
import Test.Gen.Text (genText, genUrl)
import Test.Gen.Time (genUTCTime)

--------------------------------------------------------------------------------

genEpisodeStatus :: (MonadGen m) => m Episodes.Status
genEpisodeStatus = Gen.enumBounded

episodeInsertGen :: (MonadIO m, MonadGen m) => Shows.Id -> User.Id -> m Episodes.Insert
episodeInsertGen showId userId = do
  eiId <- pure showId
  eiTitle <- genText
  eiSlug <- genSlug
  eiDescription <- Gen.maybe genText
  eiAudioFilePath <- Gen.maybe genUrl
  eiAudioFileSize <- Gen.maybe $ Gen.integral (Range.linear 1000 100000000)
  eiAudioMimeType <- Gen.maybe $ Gen.element ["audio/mpeg", "audio/mp3", "audio/wav"]
  eiDurationSeconds <- Gen.maybe $ Gen.integral (Range.linear 60 7200)
  eiArtworkUrl <- Gen.maybe genUrl
  eiScheduledAt <- Gen.maybe genUTCTime
  eiStatus <- genEpisodeStatus
  eiCreatedBy <- pure userId
  pure Episodes.Insert {..}
