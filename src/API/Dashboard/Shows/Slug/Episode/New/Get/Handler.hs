{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Slug.Episode.New.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Get.Templates.Auth (notAuthorizedTemplate, notLoggedInTemplate)
import API.Dashboard.Shows.Slug.Episode.New.Get.Templates.Form (episodeUploadForm)
import App.Common (getUserInfo, renderDashboardTemplate, renderTemplate)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad (guard)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Has (Has)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

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
handler _tracer showSlug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to episode upload" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (_user, userMetadata)
      | not (UserMetadata.isHostOrHigher userMetadata.mUserRole) -> do
          Log.logInfo "User without Host role tried to access episode upload" userMetadata.mDisplayName
          renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate
    Just (_user, userMetadata)
      | UserMetadata.isAdmin userMetadata.mUserRole -> do
          -- Admins can upload to any show
          Log.logInfo "Admin accessing episode upload form" userMetadata.mDisplayName
          execQuerySpan Shows.getAllActiveShows >>= \case
            Left _err -> do
              let content = Lucid.p_ "Failed to load shows."
              renderDashboardTemplate hxRequest userMetadata [] Nothing NavEpisodes Nothing Nothing content
            Right allShows -> do
              mResult <- execTransactionSpan $ runMaybeT $ do
                showModel <- MaybeT $ HT.statement () (Shows.getShowBySlug showSlug)
                upcomingDates <- lift $ HT.statement () (ShowSchedule.getUpcomingUnscheduledShowDates showModel.id 4)
                pure (showModel, upcomingDates)
              case mResult of
                Left _err -> do
                  let content = Lucid.p_ "Failed to load show data."
                  renderDashboardTemplate hxRequest userMetadata allShows Nothing NavEpisodes Nothing Nothing content
                Right Nothing -> do
                  let content = Lucid.p_ "Show not found."
                  renderDashboardTemplate hxRequest userMetadata allShows Nothing NavEpisodes Nothing Nothing content
                Right (Just (showModel, upcomingDates)) -> do
                  let content = episodeUploadForm showModel upcomingDates
                  renderDashboardTemplate hxRequest userMetadata allShows (Just showModel) NavEpisodes Nothing Nothing content
    Just (user, userMetadata) -> do
      -- Regular hosts - fetch their assigned shows
      Log.logInfo "Host accessing episode upload form" userMetadata.mDisplayName
      execQuerySpan (Shows.getShowsForUser (User.mId user)) >>= \case
        Left _err -> do
          let content = Lucid.p_ "Failed to load your shows."
          renderDashboardTemplate hxRequest userMetadata [] Nothing NavEpisodes Nothing Nothing content
        Right userShows -> do
          -- Verify user is host of the requested show and fetch episode data
          mResult <- execTransactionSpan $ runMaybeT $ do
            showModel <- MaybeT $ HT.statement () (Shows.getShowBySlug showSlug)
            -- Verify user is host of this show
            isHost <- lift $ HT.statement () (ShowHost.isUserHostOfShow (User.mId user) showModel.id)
            guard isHost
            upcomingDates <- lift $ HT.statement () (ShowSchedule.getUpcomingUnscheduledShowDates showModel.id 4)
            pure (showModel, upcomingDates)
          case mResult of
            Left _err -> do
              let content = Lucid.p_ "Failed to load show data."
              renderDashboardTemplate hxRequest userMetadata userShows Nothing NavEpisodes Nothing Nothing content
            Right Nothing -> do
              Log.logInfo "Show not found or user not authorized" (showSlug, User.mId user)
              let content = Lucid.p_ "Show not found or you don't have permission to upload episodes."
              renderDashboardTemplate hxRequest userMetadata userShows Nothing NavEpisodes Nothing Nothing content
            Right (Just (showModel, upcomingDates)) -> do
              Log.logInfo "Authorized user accessing episode upload form" showModel.id
              let content = episodeUploadForm showModel upcomingDates
              renderDashboardTemplate hxRequest userMetadata userShows (Just showModel) NavEpisodes Nothing Nothing content
