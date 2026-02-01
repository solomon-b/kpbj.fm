{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Slug.Edit.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Get.Templates.Form (schedulesToJson, template)
import API.Links (apiLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Reader (asks)
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

--------------------------------------------------------------------------------

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

    -- 2. Get storage backend
    backend <- asks getter

    -- 3. Check if staff for conditional rendering
    let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole

    -- 4. Fetch the show to edit
    showModel <- fetchShowOrNotFound slug

    -- 5. Fetch sidebar shows for dashboard navigation
    sidebarShows <- fetchShowsForUser user userMetadata

    -- 6. Fetch existing tags for this show
    existingTagsResult <- execQuery (Shows.getTagsForShow showModel.id)
    let existingTags = case existingTagsResult of
          Left _ -> ""
          Right tags -> Text.intercalate ", " $ map ShowTags.stName tags

    -- 7. Fetch staff-only data (schedules, hosts) if user is staff
    staffDataResult <- bool (pure (Right ("[]", [], Set.empty))) (fetchStaffData showModel.id) isStaff
    (schedulesJson, eligibleHosts, currentHostIds) <- case staffDataResult of
      Left err -> throwDatabaseError err
      Right result -> pure result

    -- 8. Render template
    let editTemplate = template backend showModel userMetadata isStaff schedulesJson eligibleHosts currentHostIds existingTags
    renderDashboardTemplate hxRequest userMetadata sidebarShows (Just showModel) NavSettings Nothing Nothing editTemplate

-- | Fetch show by slug, throwing NotFound if not found
fetchShowOrNotFound ::
  Slug ->
  AppM Shows.Model
fetchShowOrNotFound slug =
  execQuery (Shows.getShowBySlug slug) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Show"
    Right (Just showModel) -> pure showModel

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
