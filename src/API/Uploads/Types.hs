{-# LANGUAGE DeriveGeneric #-}

-- | Shared types for staged file upload API endpoints.
--
-- These types are used by the upload endpoints to receive multipart form data
-- and return JSON responses with upload tokens.
module API.Uploads.Types
  ( -- * Response Types
    UploadResponse (..),

    -- * Form Data Types
    AudioUploadForm (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import GHC.Generics (Generic)
import Servant.Multipart (FileData, FromMultipart, Mem, MultipartData, fromMultipart, lookupFile, lookupInput)

--------------------------------------------------------------------------------
-- Response Types

-- | Response returned after successfully staging an upload.
--
-- The token is used by the client to claim the upload when submitting the form.
data UploadResponse = UploadResponse
  { urToken :: StagedUploads.Token,
    urOriginalName :: Text,
    urMimeType :: Text,
    urFileSize :: Int64
  }
  deriving stock (Generic, Show, Eq)

instance ToJSON UploadResponse where
  toJSON resp =
    Aeson.object
      [ "token" Aeson..= StagedUploads.unToken (urToken resp),
        "originalName" Aeson..= urOriginalName resp,
        "mimeType" Aeson..= urMimeType resp,
        "fileSize" Aeson..= urFileSize resp
      ]

--------------------------------------------------------------------------------
-- Form Data Types

-- | Multipart form data for audio file uploads.
data AudioUploadForm = AudioUploadForm
  { aufFile :: FileData Mem,
    aufUploadType :: StagedUploads.UploadType
  }
  deriving stock (Generic)

instance FromMultipart Mem AudioUploadForm where
  fromMultipart multipartData =
    AudioUploadForm
      <$> lookupFile "file" multipartData
      <*> parseUploadType multipartData

-- | Parse upload type from form input
--
-- Currently only supports audio upload types since image uploads
-- use direct form submission instead of staged uploads.
parseUploadType :: MultipartData Mem -> Either String StagedUploads.UploadType
parseUploadType multipartData =
  case lookupInput "upload_type" multipartData of
    Left _ -> Left "upload_type is required"
    Right "episode_audio" -> Right StagedUploads.EpisodeAudio
    Right "station_id_audio" -> Right StagedUploads.StationIdAudio
    Right "ephemeral_audio" -> Right StagedUploads.EphemeralAudio
    Right other -> Left $ "Invalid upload_type: " <> Text.unpack other
