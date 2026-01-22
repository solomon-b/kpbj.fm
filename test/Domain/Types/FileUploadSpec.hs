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
      it "accepts MP3 MIME type" $ do
        isAudioFile "audio/mpeg" `shouldBe` True

      it "rejects non-MP3 audio and other MIME types" $ do
        isAudioFile "audio/wav" `shouldBe` False
        isAudioFile "audio/flac" `shouldBe` False
        isAudioFile "audio/aac" `shouldBe` False
        isAudioFile "audio/ogg" `shouldBe` False
        isAudioFile "audio/x-m4a" `shouldBe` False
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

  describe "Filename sanitization" $ do
    describe "sanitizeFileName" $ do
      it "leaves valid filenames unchanged" $ do
        sanitizeFileName "episode-1.mp3" `shouldBe` "episode-1.mp3"
        sanitizeFileName "my-show-artwork.jpg" `shouldBe` "my-show-artwork.jpg"
        sanitizeFileName "test123.wav" `shouldBe` "test123.wav"

      it "replaces colons with underscores" $ do
        sanitizeFileName "2025-12-19:10:52:1766170360.png" `shouldBe` "2025-12-19_10_52_1766170360.png"

      it "replaces other invalid characters with underscores" $ do
        sanitizeFileName "episode*1.mp3" `shouldBe` "episode_1.mp3"
        sanitizeFileName "episode?1.mp3" `shouldBe` "episode_1.mp3"
        sanitizeFileName "episode\"1.mp3" `shouldBe` "episode_1.mp3"
        sanitizeFileName "episode<1.mp3" `shouldBe` "episode_1.mp3"
        sanitizeFileName "episode>1.mp3" `shouldBe` "episode_1.mp3"
        sanitizeFileName "episode|1.mp3" `shouldBe` "episode_1.mp3"

      it "extracts base filename from paths" $ do
        sanitizeFileName "/path/to/file.mp3" `shouldBe` "file.mp3"
        sanitizeFileName "C:\\Users\\test\\file.mp3" `shouldBe` "file.mp3"
        sanitizeFileName "episode/1.mp3" `shouldBe` "1.mp3"

      it "returns 'unnamed' for empty result" $ do
        sanitizeFileName "" `shouldBe` "unnamed"
        sanitizeFileName "/" `shouldBe` "unnamed"

  describe "File validation" $ do
    describe "validateFileName" $ do
      it "accepts valid filenames" $ do
        validateFileName "episode-1.mp3" `shouldBe` Right ()
        validateFileName "my-show-artwork.jpg" `shouldBe` Right ()
        validateFileName "test123.wav" `shouldBe` Right ()

      it "rejects empty filename" $ do
        validateFileName "" `shouldBe` Left (InvalidFileName "Empty filename")

      it "accepts filenames with special characters (they get sanitized before use)" $ do
        -- These are now accepted because we sanitize before using
        validateFileName "episode:1.mp3" `shouldBe` Right ()
        validateFileName "2025-12-19:10:52:1766170360.png" `shouldBe` Right ()

      it "rejects filename that's too long" $ do
        let longFilename = Text.replicate 300 "a"
        validateFileName longFilename `shouldBe` Left (InvalidFileName "Filename too long")

    describe "validateFileType" $ do
      it "validates MP3 files for AudioBucket" $ do
        validateFileType AudioBucket "audio/mpeg" `shouldBe` Right ()
        validateFileType AudioBucket "audio/wav" `shouldBe` Left (UnsupportedFileType "Audio files only. Got: audio/wav")
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
