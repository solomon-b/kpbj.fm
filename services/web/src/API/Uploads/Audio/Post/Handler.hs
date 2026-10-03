-- | Handler for staged audio file uploads.
--
-- This endpoint allows authenticated users to upload audio files in the background
-- while filling out form metadata. The file is stored with a unique token that
-- can be used to claim the upload when the form is submitted.
module API.Uploads.Audio.Post.Handler
  ( handler,
    optionsHandler,

    -- * Exported for testing
    checkUploadLength,
  )
where

--------------------------------------------------------------------------------

import API.Uploads.Audio.Post.Route (CorsHeaders, UploadApiResponse (..))
import API.Uploads.Shared (ExtensionMapper, ProcessConfig (..), displayStagedUploadError, processStagedUpload)
import API.Uploads.Types (AudioUploadForm (..), UploadResponse (..))
import Amazonka qualified as AWS
import App.Config (Environment)
import App.Domains qualified as Domains
import App.Handler.Combinators (requireAuth)
import App.Monad (AppM)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has qualified as Has
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.BreakPlanner (psaMaxSeconds, stationIdMaxSeconds, underwritingMaxSeconds)
import Domain.Types.Cookie (Cookie)
import Domain.Types.Origin (Origin (..))
import Domain.Types.StorageBackend (StorageBackend)
import Effects.AudioDuration (probeDurationSeconds)
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.User qualified as User
import Effects.MimeTypeValidation qualified as MimeValidation
import Log qualified
import Servant qualified
import Servant.Multipart (fdFileCType, fdFileName, fdPayload)

--------------------------------------------------------------------------------
-- Origin Validation

-- | Validate that the Origin header is present and allowed.
--
-- Returns the validated origin on success, or an error message on failure.
-- Only origins in the environment's allowed list are accepted.
validateOriginStrict ::
  Maybe Origin ->
  AppM (Either Text Origin)
validateOriginStrict = \case
  Nothing -> do
    Log.logInfo "CSRF: Origin header missing (strict mode)" ("" :: Text)
    pure $ Left "Origin header required"
  Just origin -> do
    env <- asks (Has.getter @Environment)
    let allowedOrigins = Domains.allowedOrigins env
    if origin `elem` allowedOrigins
      then pure $ Right origin
      else do
        Log.logInfo "CSRF: Origin not allowed" (Aeson.object ["origin" .= display origin])
        pure $ Left "Invalid request origin"

--------------------------------------------------------------------------------
-- CORS Headers

-- | Add CORS headers to a response for a validated origin.
-- Only call this with origins that have passed validation.
addCorsHeaders ::
  Origin ->
  UploadApiResponse ->
  Servant.Headers CorsHeaders UploadApiResponse
addCorsHeaders validatedOrigin response =
  Servant.addHeader (unOrigin validatedOrigin) $
    Servant.addHeader "true" response

-- | Return response without CORS headers (browser won't be able to read it).
-- Used for invalid origins - the request fails and attacker's JS can't read why.
noCorsHeaders ::
  UploadApiResponse ->
  Servant.Headers CorsHeaders UploadApiResponse
noCorsHeaders response =
  Servant.noHeader $ Servant.noHeader response

handler ::
  Maybe Cookie ->
  Maybe Origin ->
  AudioUploadForm ->
  AppM (Servant.Headers CorsHeaders UploadApiResponse)
handler cookie mOrigin form = do
  -- Validate CSRF (Origin header required for XHR-only endpoints)
  validateOriginStrict mOrigin >>= \case
    Left err -> pure $ noCorsHeaders $ UploadError err
    Right validatedOrigin -> do
      -- Require authentication (any logged-in user can upload)
      authResult <- runExceptT $ requireAuth cookie
      case authResult of
        Left _ -> pure $ addCorsHeaders validatedOrigin $ UploadError "Authentication required"
        Right (user, _userMetadata) -> do
          result <- processAudioUpload user form
          pure $ addCorsHeaders validatedOrigin result

--------------------------------------------------------------------------------
-- CORS Preflight Handler

-- | CORS preflight response headers type.
type OptionsHeaders =
  '[ Servant.Header "Access-Control-Allow-Origin" Text,
     Servant.Header "Access-Control-Allow-Methods" Text,
     Servant.Header "Access-Control-Allow-Headers" Text,
     Servant.Header "Access-Control-Allow-Credentials" Text,
     Servant.Header "Access-Control-Max-Age" Text
   ]

-- | Handle OPTIONS preflight requests for CORS.
-- Only returns CORS headers for validated origins.
optionsHandler ::
  Maybe Origin ->
  AppM (Servant.Headers OptionsHeaders Servant.NoContent)
optionsHandler mOrigin = do
  validateOriginStrict mOrigin >>= \case
    Left _ ->
      -- Invalid origin - return empty CORS headers (preflight fails)
      pure $
        Servant.noHeader $
          Servant.noHeader $
            Servant.noHeader $
              Servant.noHeader $
                Servant.noHeader Servant.NoContent
    Right validatedOrigin ->
      -- Valid origin - return proper CORS headers
      pure $
        Servant.addHeader (unOrigin validatedOrigin) $
          Servant.addHeader "POST, OPTIONS" $
            Servant.addHeader "Content-Type, Cookie, Origin" $
              Servant.addHeader "true" $
                Servant.addHeader "86400" Servant.NoContent

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
      tempFilePath = fdPayload fileData

  let config =
        ProcessConfig
          { pcValidator = MimeValidation.validateAudioFile,
            pcExtensionMapper = audioExtensionMapper,
            pcUploadType = uploadType,
            pcLogPrefix = "Audio"
          }

  -- The length is read here rather than in the browser, so the break budget
  -- never depends on what a client reports. A file ffprobe cannot measure is
  -- one the stream cannot play, so it is refused now rather than at submit.
  -- The length goes on the staged upload row. The save handlers read it from
  -- there when they claim the upload.
  mDuration <- probeDurationSeconds tempFilePath

  case mDuration of
    Nothing -> pure $ UploadError "Could not read the length of this audio. The file may be damaged. Re-encode it and upload again."
    Just durationSeconds
      | Left message <- checkUploadLength uploadType durationSeconds ->
          pure $ UploadError message
    Just durationSeconds -> do
      result <- processStagedUpload config backend mAwsEnv user.mId originalName browserMimeType tempFilePath (Just durationSeconds)

      case result of
        Left err -> pure $ UploadError (displayStagedUploadError "Audio" err)
        Right (token, origName, mimeType, fileSize) ->
          pure $
            UploadSuccess
              UploadResponse
                { urToken = token,
                  urOriginalName = origName,
                  urMimeType = mimeType,
                  urFileSize = fileSize
                }

-- | Refuse a file that is too long for what it is uploaded as.
--
-- The upload field shows the message at once, so the user does not fill in
-- the rest of the form first. The save handlers check the same limits again.
-- An episode has no check here, because its limit depends on the slot, and
-- the slot can still change after the upload.
--
-- PSAs and underwriting announcements share one upload type. The check uses
-- the larger of their two limits, so it never refuses a file that one of them
-- accepts. The save handler applies the exact limit of the category.
checkUploadLength :: StagedUploads.UploadType -> Int64 -> Either Text ()
checkUploadLength uploadType durationSeconds =
  case uploadType of
    StagedUploads.StationIdAudio -> withinLimit "Station IDs" stationIdMaxSeconds
    StagedUploads.BreakItemAudio ->
      withinLimit "PSAs and underwriting announcements" (max psaMaxSeconds underwritingMaxSeconds)
    StagedUploads.EpisodeAudio -> Right ()
    StagedUploads.EphemeralAudio -> Right ()
  where
    withinLimit plural limit
      | durationSeconds <= limit = Right ()
      | otherwise =
          Left $
            plural
              <> " can run at most "
              <> Text.pack (show limit)
              <> " seconds. This file runs "
              <> Text.pack (show durationSeconds)
              <> "."

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
