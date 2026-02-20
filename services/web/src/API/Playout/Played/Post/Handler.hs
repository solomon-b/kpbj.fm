{-# LANGUAGE OverloadedRecordDot #-}

-- | Handler for POST /api/playout/played.
module API.Playout.Played.Post.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (PlayedRequest (..))
import App.CustomContext (PlayoutSecret (..))
import App.Handler.Error (HandlerError (..), throwHandlerFailure, throwNotAuthenticated)
import App.Monad (AppM)
import Control.Monad (unless)
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Has qualified as Has
import Data.Text (Text)
import Effects.Database.Execute (execQuery)
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

  let insert =
        PlaybackHistory.Insert
          { piTitle = request.prTitle,
            piArtist = request.prArtist,
            piSourceType = request.prSourceType,
            piSourceUrl = request.prSourceUrl,
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
