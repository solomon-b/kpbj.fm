{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Slug.Edit.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Get.Templates.Form (schedulesToJson, template)
import API.Links (apiLinks, userLinks)
import API.Types
import App.Common (getUserInfo, renderDashboardTemplate)
import Component.Banner (BannerType (..))
import Component.DashboardFrame (DashboardNav (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Bool (bool)
import Data.Has (Has)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

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
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to show edit" ()
      let banner = BannerParams Error "Login Required" "You must be logged in to edit a show."
      pure $ redirectWithBanner [i|/#{userLoginGetUrl}|] banner
    Just (_user, userMetadata)
      | isSuspended userMetadata -> do
          let banner = BannerParams Error "Account Suspended" "Your account has been suspended."
          pure $ redirectWithBanner [i|/#{rootGetUrl}|] banner
    Just (user, userMetadata) -> do
      -- Check if user can edit this show (host or staff+)
      execQuerySpan (ShowHost.isUserHostOfShowSlug user.mId slug) >>= \case
        Left err -> do
          Log.logAttention "isUserHostOfShow execution error" (show err)
          let banner = BannerParams Error "Error" "An error occurred. Please try again."
          pure $ redirectWithBanner [i|/#{rootGetUrl}|] banner
        Right isHost -> do
          let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole
          if not (isHost || isStaff)
            then do
              let banner = BannerParams Error "Access Denied" "You don't have permission to edit this show."
              pure $ redirectWithBanner [i|/#{rootGetUrl}|] banner
            else do
              -- Fetch the show to edit
              execQuerySpan (Shows.getShowBySlug slug) >>= \case
                Left err -> do
                  Log.logAttention "getShowBySlug execution error" (show err)
                  let banner = BannerParams Error "Error" "Failed to load show. Please try again."
                  pure $ redirectWithBanner [i|/#{rootGetUrl}|] banner
                Right Nothing -> do
                  Log.logInfo_ $ "No show with slug: '" <> display slug <> "'"
                  let banner = BannerParams Warning "Not Found" "The show you're trying to edit doesn't exist."
                  pure $ redirectWithBanner [i|/#{rootGetUrl}|] banner
                Right (Just showModel) -> do
                  -- Fetch sidebar shows for dashboard navigation
                  sidebarShowsResult <-
                    if UserMetadata.isAdmin userMetadata.mUserRole
                      then execQuerySpan Shows.getAllActiveShows
                      else execQuerySpan (Shows.getShowsForUser user.mId)
                  let sidebarShows = either (const []) id sidebarShowsResult
                      selectedShow = Just showModel

                  -- Fetch staff-only data (schedules, hosts) if user is staff
                  bool (pure (Right ("[]", [], Set.empty))) (fetchStaffData showModel.id) isStaff >>= \case
                    Left err -> do
                      Log.logAttention "Failed to fetchStaffData" (show err)
                      let banner = BannerParams Error "Error" "An error occurred fetching show data."
                      pure $ redirectWithBanner [i|/#{rootGetUrl}|] banner
                    Right (schedulesJson, eligibleHosts, currentHostIds) -> do
                      let editTemplate = template showModel userMetadata isStaff schedulesJson eligibleHosts currentHostIds
                      renderDashboardTemplate hxRequest userMetadata sidebarShows selectedShow NavShows Nothing Nothing editTemplate

-- | Fetch staff-only data for the edit form (schedules and hosts)
fetchStaffData ::
  ( Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadDB m,
    Has Tracer env
  ) =>
  Shows.Id ->
  m (Either HSQL.Pool.UsageError (Text, [UserMetadata.UserWithMetadata], Set User.Id))
fetchStaffData showId = runDBTransaction $ do
  schedulesJson <- TRX.statement () $ ShowSchedule.getActiveScheduleTemplatesForShow showId
  eligibleHosts <- TRX.statement () $ UserMetadata.getAllUsersWithPagination 1000 0
  currentHostIds <- TRX.statement () $ ShowHost.getShowHosts showId

  pure (schedulesToJson schedulesJson, eligibleHosts, Set.fromList $ fmap (.shmUserId) currentHostIds)
