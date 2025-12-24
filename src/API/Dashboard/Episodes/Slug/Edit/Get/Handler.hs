{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Slug.Edit.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Edit.Get.Templates.Form (template)
import API.Get.Templates qualified as HomeTemplate
import App.Common (getUserInfo, renderDashboardTemplate, renderTemplate)
import Component.DashboardFrame (DashboardNav (..))
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Has (Has)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (getCurrentTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified

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
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text, Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer showSlug episodeNumber cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to episode edit" ()
      let banner = renderBanner Warning "Login Required" "Please login to edit episodes."
      html <- renderTemplate hxRequest Nothing $ case hxRequest of
        IsHxRequest -> HomeTemplate.template <> banner
        IsNotHxRequest -> banner <> HomeTemplate.template
      pure $ Servant.addHeader "/" $ Servant.noHeader html
    Just (user, userMetadata) -> do
      -- Fetch episode by show slug and episode number
      mResult <- execTransactionSpan $ runMaybeT $ do
        episode <- MaybeT $ HT.statement () (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber)
        showResult <- MaybeT $ HT.statement () (Shows.getShowById episode.showId)
        tracks <- lift $ HT.statement () (EpisodeTrack.getTracksForEpisode episode.id)
        -- Admins don't need explicit host check since they have access to all shows
        isHost <-
          if UserMetadata.isAdmin userMetadata.mUserRole
            then pure True
            else lift $ HT.statement () (ShowHost.isUserHostOfShow user.mId episode.showId)
        MaybeT $ pure $ Just (episode, showResult, tracks, isHost)

      case mResult of
        Left err -> do
          Log.logAttention "getEpisodeByShowAndNumber execution error" (show err)
          let banner = renderBanner Warning "Episode Not Found" "The episode you're trying to edit doesn't exist."
          html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
            IsHxRequest -> HomeTemplate.template <> banner
            IsNotHxRequest -> banner <> HomeTemplate.template
          pure $ Servant.addHeader "/" $ Servant.noHeader html
        Right Nothing -> do
          Log.logInfo_ $ "No episode : show='" <> display showSlug <> "' number=" <> display episodeNumber
          let banner = renderBanner Warning "Episode Not Found" "The episode you're trying to edit doesn't exist."
          html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
            IsHxRequest -> HomeTemplate.template <> banner
            IsNotHxRequest -> banner <> HomeTemplate.template
          pure $ Servant.addHeader "/" $ Servant.noHeader html
        Right (Just (episode, showModel, tracks, isHost)) -> do
          if episode.createdBy == user.mId || isHost || UserMetadata.isStaffOrHigher userMetadata.mUserRole
            then do
              Log.logInfo "Authorized user accessing episode edit form" episode.id
              currentTime <- liftIO getCurrentTime
              Log.logInfo "Episode scheduled_at" (show episode.scheduledAt)
              Log.logInfo "Current time" (show currentTime)
              Log.logInfo "Is scheduled in future?" (show $ case episode.scheduledAt of Nothing -> True; Just s -> s > currentTime)
              -- Fetch shows for dashboard sidebar
              allShows <-
                if UserMetadata.isAdmin userMetadata.mUserRole
                  then either (const []) id <$> execQuerySpan Shows.getAllActiveShows
                  else either (const []) id <$> execQuerySpan (Shows.getShowsForUser user.mId)
              let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole
                  editTemplate = template currentTime showModel episode tracks userMetadata isStaff
              html <- renderDashboardTemplate hxRequest userMetadata allShows (Just showModel) NavEpisodes Nothing Nothing editTemplate
              pure $ Servant.noHeader $ Servant.noHeader html
            else do
              Log.logInfo "User tried to edit episode they don't own" episode.id
              let banner = renderBanner Error "Access Denied" "You can only edit your own episodes."
              html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
                IsHxRequest -> HomeTemplate.template <> banner
                IsNotHxRequest -> banner <> HomeTemplate.template
              pure $ Servant.addHeader "/" $ Servant.noHeader html
