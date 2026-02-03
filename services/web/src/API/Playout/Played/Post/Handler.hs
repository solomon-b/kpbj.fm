{-# LANGUAGE OverloadedRecordDot #-}

-- | Handler for POST /api/playout/played.
module API.Playout.Played.Post.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (PlayedRequest (..))
import App.CustomContext (PlayoutSecret (..))
import App.Monad (AppM)
import Control.Monad (unless)
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks)
import Data.Has qualified as Has
import Data.Text (Text)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.PlaybackHistory qualified as PlaybackHistory
import Log qualified
import Servant qualified
import Servant.Server (err401, err500)

--------------------------------------------------------------------------------

-- | Handler for POST /api/playout/played.
--
-- Logs a track that has started playing on the stream.
-- Validates the X-Playout-Secret header before inserting.
handler :: Maybe Text -> PlayedRequest -> AppM Servant.NoContent
handler mSecret request = do
  -- Verify the playout secret
  mExpectedSecret <- asks (Has.getter @PlayoutSecret)
  case mExpectedSecret.unPlayoutSecret of
    Nothing -> do
      Log.logAttention "PLAYOUT_SECRET not configured, rejecting request" ()
      throwM err401
    Just expected -> do
      unless (mSecret == Just expected) $ do
        Log.logAttention "Invalid or missing X-Playout-Secret header" ()
        throwM err401

  -- Insert the playback entry
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
      throwM err500
    Right () -> do
      Log.logInfo "Logged playback" request.prTitle
      pure Servant.NoContent
