{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Slug.Edit.Get.Handler (handler, action, ShowEditViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Get.Templates.Form (schedulesToJson, template)
import API.Links (apiLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Bool (bool)
import Data.Either (fromRight)
import Data.Has (getter)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as TRX
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the show edit form page.
data ShowEditViewData = ShowEditViewData
  { sevUserMetadata :: UserMetadata.Model,
    sevSidebarShows :: [Shows.Model],
    sevShowModel :: Shows.Model,
    sevBackend :: StorageBackend,
    sevIsStaff :: Bool,
    sevSchedulesJson :: Text,
    sevEligibleHosts :: [UserMetadata.UserWithMetadata],
    sevCurrentHostIds :: Set User.Id,
    sevExistingTags :: Text
  }

-- | Business logic: fetch show and staff data.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  ExceptT HandlerError AppM ShowEditViewData
action user userMetadata slug = do
  -- 1. Get storage backend
  backend <- asks getter

  -- 2. Check if staff for conditional rendering
  let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole

  -- 3. Fetch the show to edit
  showModel <- fetchShowOrNotFound slug

  -- 4. Fetch sidebar shows for dashboard navigation
  sidebarShows <- lift $ fetchShowsForUser user userMetadata

  -- 5. Fetch existing tags for this show
  existingTagsResult <- execQuery (Shows.getTagsForShow showModel.id)
  let existingTags = case existingTagsResult of
        Left _ -> ""
        Right tags -> Text.intercalate ", " $ map ShowTags.stName tags

  -- 6. Fetch staff-only data (schedules, hosts) if user is staff
  (schedulesJson, eligibleHosts, currentHostIds) <-
    fromRightM throwDatabaseError $
      lift $
        bool (pure (Right ("[]", [], Set.empty))) (fetchStaffData showModel.id) isStaff

  pure
    ShowEditViewData
      { sevUserMetadata = userMetadata,
        sevSidebarShows = sidebarShows,
        sevShowModel = showModel,
        sevBackend = backend,
        sevIsStaff = isStaff,
        sevSchedulesJson = schedulesJson,
        sevEligibleHosts = eligibleHosts,
        sevCurrentHostIds = currentHostIds,
        sevExistingTags = existingTags
      }

handler ::
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler slug cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Show edit" apiLinks.rootGet $ do
    -- 1. Require authentication and authorization (host of show or staff+)
    (user, userMetadata) <- requireAuth cookie
    requireShowHostOrStaff user.mId slug userMetadata
    vd <- action user userMetadata slug
    let editTemplate = template vd.sevBackend vd.sevShowModel vd.sevUserMetadata vd.sevIsStaff vd.sevSchedulesJson vd.sevEligibleHosts vd.sevCurrentHostIds vd.sevExistingTags
    lift $ renderDashboardTemplate hxRequest vd.sevUserMetadata vd.sevSidebarShows (Just vd.sevShowModel) NavSettings Nothing Nothing editTemplate

-- | Fetch show by slug, throwing NotFound if not found
fetchShowOrNotFound ::
  Slug ->
  ExceptT HandlerError AppM Shows.Model
fetchShowOrNotFound slug =
  fromMaybeM (throwNotFound "Show") $
    fromRightM throwDatabaseError $
      execQuery (Shows.getShowBySlug slug)

-- | Fetch shows for user based on role
fetchShowsForUser ::
  User.Model ->
  UserMetadata.Model ->
  AppM [Shows.Model]
fetchShowsForUser user userMetadata =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then fromRight [] <$> execQuery Shows.getAllActiveShows
    else fromRight [] <$> execQuery (Shows.getShowsForUser user.mId)

-- | Fetch staff-only data for the edit form (schedules and hosts)
fetchStaffData ::
  Shows.Id ->
  AppM (Either HSQL.Pool.UsageError (Text, [UserMetadata.UserWithMetadata], Set User.Id))
fetchStaffData showId = runDBTransaction $ do
  schedulesJson <- TRX.statement () $ ShowSchedule.getActiveScheduleTemplatesForShow showId
  eligibleHosts <- TRX.statement () $ UserMetadata.getAllUsersWithPagination 1000 0
  currentHostIds <- TRX.statement () $ ShowHost.getShowHosts showId

  pure (schedulesToJson schedulesJson, eligibleHosts, Set.fromList $ fmap (.shmUserId) currentHostIds)
