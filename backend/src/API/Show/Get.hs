{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Show.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showGetLink, showsGetLink)
import API.Show.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episode qualified as Episode
import Effects.Database.Tables.Show qualified as Show
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlog
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
showGetUrl :: Text -> Links.URI
showGetUrl slug = Links.linkURI $ showGetLink slug

showsGetUrl :: Links.URI
showsGetUrl = Links.linkURI $ showsGetLink Nothing Nothing Nothing Nothing

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /shows/:slug"
    ( "shows"
        :> Servant.Capture "slug" Text
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
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer slug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie $ \(fmap snd -> mUserInfo) -> do
    showResult <- execQuerySpan (Show.getShowBySlug slug)

    case showResult of
      Left err -> do
        Log.logInfo "Failed to fetch show from database" (Aeson.object ["error" .= show err])
        renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load show. Please try again.")
      Right Nothing -> do
        Log.logInfo ("Show not found: " <> slug) ()
        renderTemplate hxRequest mUserInfo (notFoundTemplate slug)
      Right (Just showModel) -> do
        episodesResult <- execQuerySpan (Episode.getEpisodesByShowId showModel.id)
        hostsResult <- execQuerySpan (Show.getShowHostsWithUsers showModel.id)
        schedulesResult <- execQuerySpan (Show.getShowSchedules showModel.id)

        -- Fetch tracks for the latest episode if episodes exist
        latestEpisodeTracks <- case episodesResult of
          Right (latestEpisode : _) -> do
            tracksResult <- execQuerySpan (Episode.getTracksForEpisode latestEpisode.id)
            pure $ case tracksResult of
              Right tracks -> Just tracks
              Left _ -> Nothing
          _ -> pure Nothing

        -- Fetch host details for the primary host
        mHostDetails <- case hostsResult of
          Right (primaryHost : _) -> do
            hostDetailsResult <- execQuerySpan (Show.getHostDetails primaryHost.userId)
            pure $ case hostDetailsResult of
              Right details -> details
              Left _ -> Nothing
          _ -> pure Nothing

        -- Fetch recent blog posts for this show
        blogPostsResult <- execQuerySpan (ShowBlog.getPublishedShowBlogPosts showModel.id 3 0)
        let blogPosts = fromRight [] blogPostsResult

        case (episodesResult, hostsResult, schedulesResult) of
          (Right episodes, Right hosts, Right schedules) -> do
            let showTemplate = template showModel episodes latestEpisodeTracks hosts schedules mHostDetails blogPosts
            renderTemplate hxRequest mUserInfo showTemplate
          _ ->
            -- If any query fails, show with empty data for the failed parts
            let episodes = fromRight [] episodesResult
                hosts = fromRight [] hostsResult
                schedules = fromRight [] schedulesResult
                showTemplate = template showModel episodes latestEpisodeTracks hosts schedules mHostDetails blogPosts
             in renderTemplate hxRequest mUserInfo showTemplate
