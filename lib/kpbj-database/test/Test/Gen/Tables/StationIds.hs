module Test.Gen.Tables.StationIds where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.User qualified as User
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.Text (genText, genUrl)

--------------------------------------------------------------------------------

stationIdInsertGen :: (MonadGen m) => User.Id -> m StationIds.Insert
stationIdInsertGen userId = do
  siiTitle <- genText
  siiAudioFilePath <- genUrl
  siiMimeType <- Gen.element ["audio/mpeg", "audio/ogg", "audio/wav"]
  siiFileSize <- Gen.integral (Range.linear 1000 10000000 :: Range.Range Int64)
  let siiCreatorId = userId
  pure StationIds.Insert {..}
