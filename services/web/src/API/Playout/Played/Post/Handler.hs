{-# LANGUAGE OverloadedRecordDot #-}

-- | Handler for POST /api/playout/played.
module API.Playout.Played.Post.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (PlayedRequest (..))
import App.BaseUrl (baseUrl)
import App.CustomContext (PlayoutSecret (..))
import App.Handler.Error (HandlerError (..), throwHandlerFailure, throwNotAuthenticated)
import App.Monad (AppM)
import App.Storage (StorageBackend (..))
import Control.Monad (unless)
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.StorageBackend (S3StorageConfig (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.PlaybackHistory qualified as PlaybackHistory
import Log qualified
import Servant qualified
import Servant.Server (err401, err500)

--------------------------------------------------------------------------------

-- | Core business logic for logging a played track.
--
-- Validates the playout secret header and inserts the playback record.
action ::
  Maybe Text ->
  PlayedRequest ->
  ExceptT HandlerError AppM ()
action mSecret request = do
  mExpectedSecret <- asks (Has.getter @PlayoutSecret)
  case mExpectedSecret.unPlayoutSecret of
    Nothing -> do
      Log.logAttention "PLAYOUT_SECRET not configured, rejecting request" ()
      throwNotAuthenticated
    Just expected -> do
      unless (mSecret == Just expected) $ do
        Log.logAttention "Invalid or missing X-Playout-Secret header" ()
        throwNotAuthenticated

  -- Resolve episode ID from source URL when source type is "episode"
  mEpisodeId <- if request.prSourceType == "episode"
    then do
      storageBackend <- lift $ asks (Has.getter @StorageBackend)
      appBaseUrl <- lift baseUrl
      let mObjectKey = extractObjectKey appBaseUrl storageBackend request.prSourceUrl
      case mObjectKey of
        Nothing -> do
          Log.logAttention "Could not extract object key from source URL" request.prSourceUrl
          pure Nothing
        Just objectKey -> do
          result <- execQuery (Episodes.getEpisodeByAudioPath objectKey)
          case result of
            Right (Just episode) -> pure (Just episode.id.unId)
            _ -> do
              Log.logAttention "No episode found for audio path" objectKey
              pure Nothing
    else pure Nothing

  let insert =
        PlaybackHistory.Insert
          { piTitle = request.prTitle,
            piArtist = request.prArtist,
            piSourceType = request.prSourceType,
            piSourceUrl = request.prSourceUrl,
            piEpisodeId = mEpisodeId,
            piStartedAt = request.prStartedAt
          }

  result <- execQuery (PlaybackHistory.insertPlayback insert)
  case result of
    Left err -> do
      Log.logAttention "Failed to insert playback history" (show err)
      throwHandlerFailure "Failed to record playback"
    Right () -> do
      Log.logInfo "Logged playback" request.prTitle

-- | Handler for POST /api/playout/played.
--
-- Logs a track that has started playing on the stream.
-- Validates the X-Playout-Secret header before inserting.
handler :: Maybe Text -> PlayedRequest -> AppM Servant.NoContent
handler mSecret request = do
  result <- runExceptT $ action mSecret request
  case result of
    Left NotAuthenticated -> throwM err401
    Left _ -> throwM err500
    Right () -> pure Servant.NoContent


-- | Extract the object key from a full media URL by stripping the storage prefix.
--
-- For S3 storage, strips @{s3BaseUrl}/@.
-- For local storage, strips @{appBaseUrl}/media/@.
-- Returns 'Nothing' if the URL does not match the expected prefix.
extractObjectKey :: Text -> StorageBackend -> Text -> Maybe Text
extractObjectKey appBaseUrl backend url = case backend of
  S3Storage config -> Text.stripPrefix (s3BaseUrl config <> "/") url
  LocalStorage _ -> Text.stripPrefix (appBaseUrl <> "/media/") url
