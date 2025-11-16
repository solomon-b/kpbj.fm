module Effects.MimeTypeValidationSpec (spec) where

--------------------------------------------------------------------------------

import Effects.MimeTypeValidation
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Effects.MimeTypeValidation" $ do
  describe "mimeTypesMatch" $ do
    it "matches identical MIME types" $ do
      mimeTypesMatch "image/jpeg" "image/jpeg" `shouldBe` True
      mimeTypesMatch "audio/mpeg" "audio/mpeg" `shouldBe` True

    it "handles case differences" $ do
      mimeTypesMatch "IMAGE/JPEG" "image/jpeg" `shouldBe` True
      mimeTypesMatch "Audio/MPEG" "audio/mpeg" `shouldBe` True

    it "strips parameters from MIME types" $ do
      mimeTypesMatch "image/jpeg" "image/jpeg; charset=binary" `shouldBe` True
      mimeTypesMatch "text/plain; charset=utf-8" "text/plain" `shouldBe` True

    it "handles common MIME type variations" $ do
      mimeTypesMatch "image/jpg" "image/jpeg" `shouldBe` True
      mimeTypesMatch "image/jpeg" "image/jpg" `shouldBe` True
      mimeTypesMatch "audio/x-m4a" "audio/mp4" `shouldBe` True
      mimeTypesMatch "audio/mp4" "audio/x-m4a" `shouldBe` True

    it "rejects different MIME types" $ do
      mimeTypesMatch "image/jpeg" "image/png" `shouldBe` False
      mimeTypesMatch "audio/mpeg" "video/mp4" `shouldBe` False

  describe "allowedAudioTypes" $ do
    it "includes common audio formats" $ do
      allowedAudioTypes `shouldContain` ["audio/mpeg"]
      allowedAudioTypes `shouldContain` ["audio/wav"]
      allowedAudioTypes `shouldContain` ["audio/flac"]
      allowedAudioTypes `shouldContain` ["audio/aac"]
      allowedAudioTypes `shouldContain` ["audio/ogg"]
      allowedAudioTypes `shouldContain` ["audio/x-m4a"]
      allowedAudioTypes `shouldContain` ["audio/mp4"]

  describe "allowedImageTypes" $ do
    it "includes common image formats" $ do
      allowedImageTypes `shouldContain` ["image/jpeg"]
      allowedImageTypes `shouldContain` ["image/jpg"]
      allowedImageTypes `shouldContain` ["image/png"]
      allowedImageTypes `shouldContain` ["image/webp"]
      allowedImageTypes `shouldContain` ["image/gif"]
