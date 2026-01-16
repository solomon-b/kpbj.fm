module Test.Gen.Tables.StagedUploads where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.User qualified as User
import Effects.StagedUploads (generateSecureToken)
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

-- | Partial insert data that can be completed with a user ID and token
data PartialStagedUploadInsert = PartialStagedUploadInsert
  { psiOriginalName :: Text,
    psiStoragePath :: Text,
    psiMimeType :: Text,
    psiFileSize :: Int64,
    psiUploadType :: StagedUploads.UploadType
  }
  deriving stock (Show, Eq)

-- | Generate partial insert data (doesn't need MonadIO)
partialStagedUploadInsertGen :: (MonadGen m) => m PartialStagedUploadInsert
partialStagedUploadInsertGen = do
  psiOriginalName <- genOriginalFilename
  psiStoragePath <- genStoragePath
  psiMimeType <- genMimeType
  psiFileSize <- genFileSize
  psiUploadType <- genUploadType
  pure PartialStagedUploadInsert {..}

-- | Complete a partial insert with user ID and token
completeInsert :: User.Id -> StagedUploads.Token -> PartialStagedUploadInsert -> StagedUploads.Insert
completeInsert userId token partial =
  StagedUploads.Insert
    { siToken = token,
      siUserId = userId,
      siOriginalName = psiOriginalName partial,
      siStoragePath = psiStoragePath partial,
      siMimeType = psiMimeType partial,
      siFileSize = psiFileSize partial,
      siUploadType = psiUploadType partial
    }

-- | Generate a StagedUploads.Insert with the given user ID
stagedUploadInsertGen :: (MonadIO m, MonadGen m) => User.Id -> m StagedUploads.Insert
stagedUploadInsertGen userId = do
  siToken <- liftIO generateSecureToken
  siOriginalName <- genOriginalFilename
  siStoragePath <- genStoragePath
  siMimeType <- genMimeType
  siFileSize <- genFileSize
  siUploadType <- genUploadType
  pure
    StagedUploads.Insert
      { siToken = siToken,
        siUserId = userId,
        siOriginalName = siOriginalName,
        siStoragePath = siStoragePath,
        siMimeType = siMimeType,
        siFileSize = siFileSize,
        siUploadType = siUploadType
      }

-- | Generate a realistic original filename
genOriginalFilename :: (MonadGen m) => m Text
genOriginalFilename = do
  name <- Gen.text (Range.linear 3 30) Gen.alphaNum
  ext <- Gen.element [".mp3", ".wav", ".flac", ".jpg", ".png", ".webp"]
  pure $ name <> ext

-- | Generate a storage path
genStoragePath :: (MonadGen m) => m Text
genStoragePath = do
  year <- Gen.int (Range.linear 2024 2030)
  month <- Gen.int (Range.linear 1 12)
  day <- Gen.int (Range.linear 1 28)
  filename <- Gen.text (Range.linear 10 30) Gen.alphaNum
  ext <- Gen.element [".mp3", ".jpg", ".png"]
  let monthStr = if month < 10 then "0" <> Text.pack (show month) else Text.pack (show month)
      dayStr = if day < 10 then "0" <> Text.pack (show day) else Text.pack (show day)
  pure $ "temp/" <> Text.pack (show year) <> "/" <> monthStr <> "/" <> dayStr <> "/" <> filename <> ext

-- | Generate a MIME type
genMimeType :: (MonadGen m) => m Text
genMimeType =
  Gen.element
    [ "audio/mpeg",
      "audio/wav",
      "audio/flac",
      "image/jpeg",
      "image/png",
      "image/webp"
    ]

-- | Generate a realistic file size (between 1KB and 100MB)
genFileSize :: (MonadGen m) => m Int64
genFileSize = Gen.int64 (Range.linear 1024 (100 * 1024 * 1024))

-- | Generate an upload type
genUploadType :: (MonadGen m) => m StagedUploads.UploadType
genUploadType = Gen.element [StagedUploads.EpisodeAudio]
