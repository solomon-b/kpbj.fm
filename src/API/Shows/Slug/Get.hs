{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showGetLink, showsGetLink)
import API.Shows.Slug.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
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
showGetUrl :: Slug -> Links.URI
showGetUrl slug = Links.linkURI $ showGetLink slug

showsGetUrl :: Links.URI
showsGetUrl = Links.linkURI $ showsGetLink Nothing Nothing Nothing Nothing

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /shows/:slug"
    ( "shows"
        :> Servant.Capture "slug" Slug
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
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer slug cookie (foldHxReq -> hxRequest) = do
  userInfoResult <- getUserInfo cookie
  let mUserInfo = fmap snd userInfoResult
  showResult <- execQuerySpan (Shows.getShowBySlug slug)
  case showResult of
    Left err -> do
      Log.logInfo "Failed to fetch show from database" (show err)
      renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load show. Please try again.")
    Right Nothing -> do
      Log.logInfo ("Show not found: " <> display slug) ()
      renderTemplate hxRequest mUserInfo (notFoundTemplate slug)
    Right (Just showModel) -> do
      episodesResult <- execQuerySpan (Episodes.getEpisodesById showModel.id)
      hostsResult <- execQuerySpan (Shows.getShowHostsWithUsers showModel.id)
      schedulesResult <- execQuerySpan (Shows.getShowSchedules showModel.id)

      -- Check if current user can edit this show
      canEdit <- case userInfoResult of
        Nothing -> pure False
        Just (user, userMeta) -> do
          -- User must be a host of the show OR staff+
          if UserMetadata.isStaffOrHigher userMeta.mUserRole
            then pure True
            else do
              isHostResult <- execQuerySpan (Shows.isUserHostOfShow user.mId showModel.id)
              pure $ fromRight False isHostResult

      -- Fetch tracks for the latest episode if episodes exist
      latestEpisodeTracks <- case episodesResult of
        Right (latestEpisode : _) -> do
          tracksResult <- execQuerySpan (Episodes.getTracksForEpisode latestEpisode.id)
          pure $ case tracksResult of
            Right tracks -> Just tracks
            Left _ -> Nothing
        _ -> pure Nothing

      -- Fetch host details for the primary host
      mHostDetails <- case hostsResult of
        Right (ShowHost.ShowHostWithUser {userId = uid} : _) -> do
          hostDetailsResult <- execQuerySpan (Shows.getHostDetails uid)
          pure $ case hostDetailsResult of
            Right details -> details
            Left _ -> Nothing
        _ -> pure Nothing

      -- Fetch recent blog posts for this show
      blogPostsResult <- execQuerySpan (ShowBlogPosts.getPublishedShowBlogPosts showModel.id 3 0)
      let blogPosts = fromRight [] blogPostsResult

      case (episodesResult, hostsResult, schedulesResult) of
        (Right episodes, Right hosts, Right schedules) -> do
          let showTemplate = template showModel episodes latestEpisodeTracks hosts schedules mHostDetails blogPosts canEdit
          renderTemplate hxRequest mUserInfo showTemplate
        _ ->
          -- If any query fails, show with empty data for the failed parts
          let episodes = fromRight [] episodesResult
              hosts = fromRight [] hostsResult
              schedules = fromRight [] schedulesResult
              showTemplate = template showModel episodes latestEpisodeTracks hosts schedules mHostDetails blogPosts canEdit
           in renderTemplate hxRequest mUserInfo showTemplate
