{-# LANGUAGE OverloadedRecordDot #-}

module Domain.Types.FileUploadSpec (spec) where

--------------------------------------------------------------------------------

import Data.Text qualified as Text
import Domain.Types.FileStorage (BucketType (..))
import Domain.Types.FileUpload
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Domain.Types.FileUpload" $ do
  describe "File type validation" $ do
    describe "isAudioFile" $ do
      it "accepts valid audio MIME types" $ do
        isAudioFile "audio/mpeg" `shouldBe` True
        isAudioFile "audio/wav" `shouldBe` True
        isAudioFile "audio/flac" `shouldBe` True
        isAudioFile "audio/aac" `shouldBe` True
        isAudioFile "audio/ogg" `shouldBe` True
        isAudioFile "audio/x-m4a" `shouldBe` True

      it "rejects non-audio MIME types" $ do
        isAudioFile "image/jpeg" `shouldBe` False
        isAudioFile "video/mp4" `shouldBe` False
        isAudioFile "text/plain" `shouldBe` False
        isAudioFile "application/pdf" `shouldBe` False

    describe "isImageFile" $ do
      it "accepts valid image MIME types" $ do
        isImageFile "image/jpeg" `shouldBe` True
        isImageFile "image/jpg" `shouldBe` True
        isImageFile "image/png" `shouldBe` True
        isImageFile "image/webp" `shouldBe` True
        isImageFile "image/gif" `shouldBe` True

      it "rejects non-image MIME types" $ do
        isImageFile "audio/mpeg" `shouldBe` False
        isImageFile "video/mp4" `shouldBe` False
        isImageFile "text/plain" `shouldBe` False

  describe "File validation" $ do
    describe "validateFileName" $ do
      it "accepts valid filenames" $ do
        validateFileName "episode-1.mp3" `shouldBe` Right ()
        validateFileName "my-show-artwork.jpg" `shouldBe` Right ()
        validateFileName "test123.wav" `shouldBe` Right ()

      it "rejects empty filename" $ do
        validateFileName "" `shouldBe` Left (InvalidFileName "Empty filename")

      it "rejects filename with invalid characters" $ do
        validateFileName "episode/1.mp3" `shouldBe` Left (InvalidFileName "Filename contains invalid characters")
        validateFileName "episode\\1.mp3" `shouldBe` Left (InvalidFileName "Filename contains invalid characters")
        validateFileName "episode:1.mp3" `shouldBe` Left (InvalidFileName "Filename contains invalid characters")
        validateFileName "episode*.mp3" `shouldBe` Left (InvalidFileName "Filename contains invalid characters")
        validateFileName "episode?.mp3" `shouldBe` Left (InvalidFileName "Filename contains invalid characters")
        validateFileName "episode\"1.mp3" `shouldBe` Left (InvalidFileName "Filename contains invalid characters")
        validateFileName "episode<1.mp3" `shouldBe` Left (InvalidFileName "Filename contains invalid characters")
        validateFileName "episode>1.mp3" `shouldBe` Left (InvalidFileName "Filename contains invalid characters")
        validateFileName "episode|1.mp3" `shouldBe` Left (InvalidFileName "Filename contains invalid characters")

      it "rejects filename that's too long" $ do
        let longFilename = Text.replicate 300 "a"
        validateFileName longFilename `shouldBe` Left (InvalidFileName "Filename too long")

    describe "validateFileType" $ do
      it "validates audio files for AudioBucket" $ do
        validateFileType AudioBucket "audio/mpeg" `shouldBe` Right ()
        validateFileType AudioBucket "audio/wav" `shouldBe` Right ()
        validateFileType AudioBucket "image/jpeg" `shouldBe` Left (UnsupportedFileType "Audio files only. Got: image/jpeg")

      it "validates image files for ImageBucket" $ do
        validateFileType ImageBucket "image/jpeg" `shouldBe` Right ()
        validateFileType ImageBucket "image/png" `shouldBe` Right ()
        validateFileType ImageBucket "audio/mpeg" `shouldBe` Left (UnsupportedFileType "Image files only. Got: audio/mpeg")

    describe "validateFileSize" $ do
      it "accepts files within size limits" $ do
        validateFileSize AudioBucket (100 * 1024 * 1024) `shouldBe` Right () -- 100MB audio
        validateFileSize ImageBucket (5 * 1024 * 1024) `shouldBe` Right () -- 5MB image
      it "rejects files exceeding size limits" $ do
        let tooLargeAudio = maxAudioFileSize + 1
        validateFileSize AudioBucket tooLargeAudio `shouldBe` Left (FileTooLarge tooLargeAudio maxAudioFileSize)

        let tooLargeImage = maxImageFileSize + 1
        validateFileSize ImageBucket tooLargeImage `shouldBe` Left (FileTooLarge tooLargeImage maxImageFileSize)

    describe "validateUpload integration" $ do
      it "validates complete audio upload" $ do
        validateUpload AudioBucket "episode1.mp3" "audio/mpeg" (100 * 1024 * 1024) `shouldBe` Right ()

      it "validates complete image upload" $ do
        validateUpload ImageBucket "artwork.jpg" "image/jpeg" (5 * 1024 * 1024) `shouldBe` Right ()

      it "rejects invalid complete upload" $ do
        -- Wrong file type
        validateUpload AudioBucket "episode1.mp3" "image/jpeg" (100 * 1024 * 1024)
          `shouldBe` Left (UnsupportedFileType "Audio files only. Got: image/jpeg")

        -- File too large
        validateUpload AudioBucket "episode1.mp3" "audio/mpeg" (maxAudioFileSize + 1)
          `shouldSatisfy` \case
            Left (FileTooLarge _ _) -> True
            _ -> False

  describe "UploadResult type" $ do
    it "has correct field accessors" $ do
      let result = UploadResult "test.mp3" "/path/test.mp3" "audio/mpeg" 1000
      result.uploadResultOriginalName `shouldBe` "test.mp3"
      result.uploadResultStoragePath `shouldBe` "/path/test.mp3"
      result.uploadResultMimeType `shouldBe` "audio/mpeg"
      result.uploadResultFileSize `shouldBe` 1000
