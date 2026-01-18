{-# LANGUAGE QuasiQuotes #-}

-- | Handler for staged audio file uploads.
--
-- This endpoint allows authenticated users to upload audio files in the background
-- while filling out form metadata. The file is stored with a unique token that
-- can be used to claim the upload when the form is submitted.
module API.Uploads.Audio.Post.Handler where

--------------------------------------------------------------------------------

import API.Uploads.Audio.Post.Route (UploadApiResponse (..))
import API.Uploads.Shared (ExtensionMapper, ProcessConfig (..), processStagedUpload)
import API.Uploads.Types (AudioUploadForm (..), UploadResponse (..))
import Amazonka qualified as AWS
import App.Handler.Combinators (requireAuth)
import App.Monad (AppM)
import Control.Exception (SomeException)
import Control.Monad.Catch (try)
import Control.Monad.Reader (asks)
import Data.ByteString.Lazy qualified as BSL
import Data.Has qualified as Has
import Domain.Types.Cookie (Cookie)
import Domain.Types.Origin (Origin)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.CSRF (validateOriginStrict)
import Effects.Database.Tables.User qualified as User
import Effects.MimeTypeValidation qualified as MimeValidation
import OpenTelemetry.Trace (Tracer)
import Servant.Multipart (fdFileCType, fdFileName, fdPayload)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Maybe Cookie ->
  Maybe Origin ->
  AudioUploadForm ->
  AppM UploadApiResponse
handler _tracer cookie mOrigin form = do
  -- Validate CSRF (Origin header required for XHR-only endpoints)
  -- Localhost origins (any port) are automatically allowed for development.
  validateOriginStrict mOrigin >>= \case
    Left err -> pure $ UploadError err
    Right () -> do
      -- Require authentication (any logged-in user can upload)
      authResult <- try @_ @SomeException $ requireAuth cookie
      case authResult of
        Left _ -> pure $ UploadError "Authentication required"
        Right (user, _userMetadata) -> do
          processAudioUpload user form

--------------------------------------------------------------------------------

-- | Process the audio file upload.
processAudioUpload ::
  User.Model ->
  AudioUploadForm ->
  AppM UploadApiResponse
processAudioUpload user form = do
  backend <- asks (Has.getter @StorageBackend)
  mAwsEnv <- asks (Has.getter @(Maybe AWS.Env))

  let fileData = aufFile form
      uploadType = aufUploadType form
      originalName = fdFileName fileData
      browserMimeType = fdFileCType fileData
      content = BSL.toStrict $ fdPayload fileData

  let config =
        ProcessConfig
          { pcValidator = MimeValidation.validateAudioFile,
            pcExtensionMapper = audioExtensionMapper,
            pcUploadType = uploadType,
            pcTempFilePrefix = "staged-audio-",
            pcLogPrefix = "Audio"
          }

  result <- processStagedUpload config backend mAwsEnv user.mId originalName browserMimeType content

  case result of
    Left err -> pure $ UploadError err
    Right (token, origName, mimeType, fileSize) ->
      pure $
        UploadSuccess
          UploadResponse
            { urToken = token,
              urOriginalName = origName,
              urMimeType = mimeType,
              urFileSize = fileSize
            }

--------------------------------------------------------------------------------
-- Audio-specific configuration

-- | Map MIME type to file extension for audio files.
audioExtensionMapper :: ExtensionMapper
audioExtensionMapper mimeType = case mimeType of
  "audio/mpeg" -> "mp3"
  "audio/wav" -> "wav"
  "audio/flac" -> "flac"
  "audio/aac" -> "aac"
  "audio/ogg" -> "ogg"
  "audio/x-m4a" -> "m4a"
  _ -> "bin"
