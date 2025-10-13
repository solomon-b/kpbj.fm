{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Episode.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodeGetLink, showGetLink)
import API.Episode.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- URL helpers
episodeGetUrl :: Text -> Text -> Links.URI
episodeGetUrl showSlug episodeSlug = Links.linkURI $ episodeGetLink showSlug episodeSlug

showGetUrl :: Text -> Links.URI
showGetUrl slug = Links.linkURI $ showGetLink slug

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /shows/:show_slug/episodes/:episode_slug"
    ( "shows"
        :> Servant.Capture "show_slug" Text
        :> "episodes"
        :> Servant.Capture "episode_slug" Text
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Text ->
  Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showSlug episodeSlug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \(fmap snd -> mUserInfo) -> do
    execQuerySpan (Episodes.getEpisodeBySlug showSlug episodeSlug) >>= \case
      Left err -> do
        Log.logInfo "Failed to fetch episode from database" (Aeson.object ["error" .= show err])
        renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load episode. Please try again.")
      Right Nothing -> do
        Log.logInfo ("Episode not found: " <> showSlug <> "/" <> episodeSlug) ()
        renderTemplate hxRequest mUserInfo (notFoundTemplate showSlug episodeSlug)
      Right (Just episode) -> do
        -- Fetch the show
        showResult <- execQuerySpan (Shows.getShowById episode.showId)

        -- Fetch tracks for the episode
        tracksResult <- execQuerySpan (Episodes.getTracksForEpisode episode.id)

        case showResult of
          Left err -> do
            Log.logInfo "Failed to fetch show from database" (Aeson.object ["error" .= show err])
            renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load show data. Please try again.")
          Right Nothing -> do
            Log.logInfo ("Show not found for episode: " <> showSlug) ()
            renderTemplate hxRequest mUserInfo (errorTemplate "Show not found for this episode.")
          Right (Just showModel) -> do
            let tracks = case tracksResult of
                  Right ts -> ts
                  Left _ -> []
                episodeTemplate = template showModel episode tracks
            renderTemplate hxRequest mUserInfo episodeTemplate
