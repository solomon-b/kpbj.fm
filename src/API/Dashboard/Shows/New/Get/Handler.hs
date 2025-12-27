{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.New.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.New.Get.Templates.Page (template)
import API.Links (apiLinks, userLinks)
import API.Types (Routes (..), UserRoutes (..))
import App.Common (getUserInfo, renderDashboardTemplate, renderTemplate)
import Component.Banner (BannerType (..))
import Component.DashboardFrame (DashboardNav (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
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
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      let banner = BannerParams Error "Login Required" "You must be logged in to access this page."
      renderTemplate hxRequest Nothing (redirectWithBanner [i|/#{userLoginGetUrl}|] banner)
    Just (_user, userMetadata) ->
      if not (UserMetadata.isAdmin userMetadata.mUserRole) || isSuspended userMetadata
        then do
          let banner = BannerParams Error "Admin Access Required" "You do not have permission to access this page."
          renderTemplate hxRequest (Just userMetadata) (redirectWithBanner [i|/#{rootGetUrl}|] banner)
        else do
          -- Fetch all users for the host selection dropdown
          execQuerySpan (UserMetadata.getAllUsersWithPagination 1000 0) >>= \case
            Left _err -> do
              Log.logInfo "Failed to fetch eligible hosts from database" ()
              let banner = BannerParams Error "Error" "Failed to load form data. Please try again."
              renderTemplate hxRequest (Just userMetadata) (redirectWithBanner [i|/#{rootGetUrl}|] banner)
            Right eligibleHosts -> do
              renderDashboardTemplate hxRequest userMetadata [] Nothing NavShows Nothing Nothing (template eligibleHosts)
