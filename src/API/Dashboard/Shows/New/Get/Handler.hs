{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.New.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.New.Get.Templates.Page (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.GoogleAnalyticsId (GoogleAnalyticsId)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
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
    Has HSQL.Pool.Pool env,
    Has (Maybe GoogleAnalyticsId) env
  ) =>
  Tracer ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "New show form" apiLinks.rootGet $ do
    -- 1. Require authentication and admin role
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Fetch all users for the host selection dropdown
    eligibleHostsResult <- execQuerySpan (UserMetadata.getAllUsersWithPagination 1000 0)
    eligibleHosts <- case eligibleHostsResult of
      Left err -> throwDatabaseError err
      Right hosts -> pure hosts

    -- 3. Render form
    renderDashboardTemplate hxRequest userMetadata [] Nothing NavShows Nothing Nothing (template eligibleHosts)
