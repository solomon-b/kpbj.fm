module Test.Gen.Tables.Episodes where

--------------------------------------------------------------------------------

import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.Text (genText, genUrl)
import Test.Gen.Time (genUTCTime)

--------------------------------------------------------------------------------

episodeInsertGen :: (MonadGen m) => Shows.Id -> ShowSchedule.TemplateId -> User.Id -> m Episodes.Insert
episodeInsertGen showId templateId userId = do
  let eiId = showId
  let eiScheduleTemplateId = templateId
  eiDescription <- Gen.maybe genText
  eiAudioFilePath <- Gen.maybe genUrl
  eiAudioFileSize <- Gen.maybe $ Gen.integral (Range.linear 1000 100000000)
  eiAudioMimeType <- Gen.maybe $ Gen.element ["audio/mpeg", "audio/mp3", "audio/wav"]
  eiDurationSeconds <- Gen.maybe $ Gen.integral (Range.linear 60 7200)
  eiArtworkUrl <- Gen.maybe genUrl
  eiScheduledAt <- genUTCTime
  let eiCreatedBy = userId
  pure Episodes.Insert {..}
