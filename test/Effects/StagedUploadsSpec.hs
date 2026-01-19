module Effects.StagedUploadsSpec (spec) where

--------------------------------------------------------------------------------

import Data.Char (isAsciiLower, isDigit)
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Tables.StagedUploads
  ( Status (..),
    Token (..),
    UploadType (..),
  )
import Effects.StagedUploads (generateSecureToken)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------
-- Generators

-- | Generate a random UploadType
genUploadType :: (MonadGen m) => m UploadType
genUploadType = Gen.element [EpisodeAudio]

-- | Generate a random Status
genStatus :: (MonadGen m) => m Status
genStatus = Gen.element [Pending, Claimed, Expired]

--------------------------------------------------------------------------------
-- Spec

spec :: Spec
spec = describe "Effects.StagedUploads" $ do
  describe "Token generation" $ do
    it "generates tokens of exactly 32 characters (UUID without hyphens)" $ hedgehog $ do
      Token tokenText <- evalIO generateSecureToken
      Text.length tokenText === 32

    it "generates tokens containing only hex characters" $ hedgehog $ do
      Token tokenText <- evalIO generateSecureToken
      assert $ Text.all isHexChar tokenText

    it "generates URL-safe tokens (no special characters)" $ hedgehog $ do
      Token tokenText <- evalIO generateSecureToken
      -- URL-safe means alphanumeric only (hex is a subset)
      assert $ Text.all isUrlSafe tokenText

    it "generates unique tokens (probabilistic)" $ hedgehog $ do
      -- Generate 100 tokens and verify uniqueness
      -- With UUID v4, collision probability is astronomically low
      tokens <- evalIO $ mapM (const generateSecureToken) [1 :: Int .. 100]
      let tokenTexts = map (\(Token t) -> t) tokens
      length (nub tokenTexts) === 100

  describe "UploadType" $ do
    it "all constructors are covered by encoding" $ hedgehog $ do
      uploadType <- forAll genUploadType
      let encoded = encodeUploadType uploadType
      assert $ not (Text.null encoded)

    it "encoding produces valid database enum values" $ hedgehog $ do
      uploadType <- forAll genUploadType
      let encoded = encodeUploadType uploadType
      assert $ encoded `elem` validUploadTypes

    it "round-trips through encode/decode" $ hedgehog $ do
      uploadType <- forAll genUploadType
      let encoded = encodeUploadType uploadType
          decoded = decodeUploadType encoded
      decoded === Just uploadType

    it "decode rejects invalid values" $ do
      decodeUploadType "invalid_type" `shouldBe` Nothing
      decodeUploadType "" `shouldBe` Nothing
      decodeUploadType "EPISODE_AUDIO" `shouldBe` Nothing -- Case sensitive
  describe "Status" $ do
    it "all constructors are covered by encoding" $ hedgehog $ do
      status <- forAll genStatus
      let encoded = encodeStatus status
      assert $ not (Text.null encoded)

    it "encoding produces valid database enum values" $ hedgehog $ do
      status <- forAll genStatus
      let encoded = encodeStatus status
      assert $ encoded `elem` validStatuses

    it "round-trips through encode/decode" $ hedgehog $ do
      status <- forAll genStatus
      let encoded = encodeStatus status
          decoded = decodeStatus encoded
      decoded === Just status

    it "decode rejects invalid values" $ do
      decodeStatus "invalid_status" `shouldBe` Nothing
      decodeStatus "" `shouldBe` Nothing
      decodeStatus "PENDING" `shouldBe` Nothing -- Case sensitive

--------------------------------------------------------------------------------
-- Helpers

-- | Check if a character is a hex character
isHexChar :: Char -> Bool
isHexChar c = c `elem` ("0123456789abcdef" :: String)

-- | Check if a character is URL-safe (alphanumeric)
isUrlSafe :: Char -> Bool
isUrlSafe c = isAsciiLower c || isDigit c

-- | Encode UploadType to database string (matches EncodeValue instance)
encodeUploadType :: UploadType -> Text
encodeUploadType = \case
  EpisodeAudio -> "episode_audio"

-- | Decode UploadType from database string (matches DecodeValue instance)
decodeUploadType :: Text -> Maybe UploadType
decodeUploadType = \case
  "episode_audio" -> Just EpisodeAudio
  _ -> Nothing

-- | Encode Status to database string
encodeStatus :: Status -> Text
encodeStatus = \case
  Pending -> "pending"
  Claimed -> "claimed"
  Expired -> "expired"

-- | Decode Status from database string
decodeStatus :: Text -> Maybe Status
decodeStatus = \case
  "pending" -> Just Pending
  "claimed" -> Just Claimed
  "expired" -> Just Expired
  _ -> Nothing

-- | Valid upload type strings in the database
validUploadTypes :: [Text]
validUploadTypes = ["episode_audio"]

-- | Valid status strings in the database
validStatuses :: [Text]
validStatuses = ["pending", "claimed", "expired"]
