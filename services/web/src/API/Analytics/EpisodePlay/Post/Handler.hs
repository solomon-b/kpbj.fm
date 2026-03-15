{-# LANGUAGE OverloadedRecordDot #-}

-- | Handler for POST /api/analytics/episode-play.
module API.Analytics.EpisodePlay.Post.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import API.Analytics.EpisodePlay.Post.Route (EpisodePlayRequest (..))
import App.Monad (AppM)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EpisodePlayEvents qualified as EpisodePlayEvents
import Effects.Database.Tables.Episodes qualified as Episodes
import Log qualified
import Servant qualified

--------------------------------------------------------------------------------

-- | Handler for POST /api/analytics/episode-play.
--
-- Records that a user played an archived episode. No authentication required.
-- Relies on the FK constraint on @episode_id@ to reject invalid episode IDs.
handler :: EpisodePlayRequest -> AppM Servant.NoContent
handler request = do
  let eid = Episodes.Id request.episodeId
  result <- execQuery (EpisodePlayEvents.insertPlayEvent eid)
  case result of
    Right () ->
      Log.logInfo "Recorded episode play" request.episodeId
    Left err ->
      Log.logAttention "Failed to insert episode play event" (show err)
  pure Servant.NoContent
