module Test.Gen.Tables.EphemeralUploads where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.User qualified as User
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.Text (genText, genUrl)

--------------------------------------------------------------------------------

ephemeralUploadInsertGen :: (MonadGen m) => User.Id -> m EphemeralUploads.Insert
ephemeralUploadInsertGen userId = do
  euiTitle <- genText
  euiAudioFilePath <- genUrl
  euiMimeType <- Gen.element ["audio/mpeg", "audio/ogg", "audio/wav"]
  euiFileSize <- Gen.integral (Range.linear 1000 10000000 :: Range.Range Int64)
  let euiCreatorId = userId
  pure EphemeralUploads.Insert {..}
