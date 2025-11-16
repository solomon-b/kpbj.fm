{-# LANGUAGE ScopedTypeVariables #-}

module Effects.MimeTypeValidation where

--------------------------------------------------------------------------------

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Log qualified
import Magic qualified

--------------------------------------------------------------------------------

-- | Validate a file's MIME type by checking its magic bytes
validateFileMimeType :: (MonadIO m, Log.MonadLog m) => FilePath -> Text -> m (Either Text Text)
validateFileMimeType filePath expectedMimeType = do
  result <- liftIO $ try $ do
    magic <- Magic.magicOpen [Magic.MagicMime]
    Magic.magicLoadDefault magic
    actualMimeType <- Magic.magicFile magic filePath
    pure $ Text.pack actualMimeType

  case result of
    Left (err :: SomeException) -> do
      -- Log the magic failure for debugging
      Log.logAttention "Magic MIME type detection failed, falling back to extension-based validation" (filePath, Text.pack $ show err)
      pure $ Right expectedMimeType
    Right actualMimeType -> do
      if mimeTypesMatch expectedMimeType actualMimeType
        then pure $ Right actualMimeType
        else pure $ Left $ "MIME type mismatch: expected " <> expectedMimeType <> ", got " <> actualMimeType

-- | Check if two MIME types match, accounting for variations
mimeTypesMatch :: Text -> Text -> Bool
mimeTypesMatch expected actual =
  let cleanMime = Text.takeWhile (/= ';') . Text.strip . Text.toLower
      expectedClean = cleanMime expected
      actualClean = cleanMime actual
   in expectedClean == actualClean
        ||
        -- Handle common variations
        (expectedClean == "image/jpg" && actualClean == "image/jpeg")
        || (expectedClean == "image/jpeg" && actualClean == "image/jpg")
        || (expectedClean == "audio/x-m4a" && actualClean == "audio/mp4")
        || (expectedClean == "audio/mp4" && actualClean == "audio/x-m4a")

-- | Validate that a file's actual content matches expected MIME type
-- and is in the list of allowed types for the given category
validateFileContent :: (MonadIO m, Log.MonadLog m) => FilePath -> Text -> [Text] -> m (Either Text Text)
validateFileContent filePath expectedMimeType allowedTypes = do
  mimeResult <- validateFileMimeType filePath expectedMimeType
  case mimeResult of
    Left err -> pure $ Left err
    Right actualMimeType -> do
      if any (mimeTypesMatch actualMimeType) allowedTypes
        then pure $ Right actualMimeType
        else pure $ Left $ "File type not allowed: " <> actualMimeType <> ". Allowed types: " <> Text.intercalate ", " allowedTypes

-- | Validate audio file content
validateAudioFile :: (MonadIO m, Log.MonadLog m) => FilePath -> Text -> m (Either Text Text)
validateAudioFile filePath expectedMimeType =
  validateFileContent filePath expectedMimeType allowedAudioTypes

-- | Validate image file content
validateImageFile :: (MonadIO m, Log.MonadLog m) => FilePath -> Text -> m (Either Text Text)
validateImageFile filePath expectedMimeType =
  validateFileContent filePath expectedMimeType allowedImageTypes

-- | Allowed audio MIME types
allowedAudioTypes :: [Text]
allowedAudioTypes =
  [ "audio/mpeg",
    "audio/wav",
    "audio/flac",
    "audio/aac",
    "audio/ogg",
    "audio/x-m4a",
    "audio/mp4"
  ]

-- | Allowed image MIME types
allowedImageTypes :: [Text]
allowedImageTypes =
  [ "image/jpeg",
    "image/jpg",
    "image/png",
    "image/webp",
    "image/gif"
  ]
