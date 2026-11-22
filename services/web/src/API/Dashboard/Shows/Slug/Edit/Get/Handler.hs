{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Slug.Edit.Get.Handler (handler, action, ShowEditViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Get.Templates.Form (template)
import API.Links (apiLinks)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Combinators (authorizeShow)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.ScheduleEditor (schedulesToEditorJson)
import Control.Monad (void)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Bool (bool)
import Data.Has (getter)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Domain.Types.Timezone (pacificDay)
import Effects.Clock (currentSystemTime)
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
import Rel8 (Result)
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the show edit form page.
data ShowEditViewData = ShowEditViewData
  { sevUserMetadata :: UserMetadata.Model,
    sevShowModel :: Shows.Model,
    sevBackend :: StorageBackend,
    sevIsStaff :: Bool,
    sevSchedulesJson :: Text,
    sevEligibleHosts :: [UserMetadata.UserWithMetadata],
    sevCurrentHostIds :: Set User.Id,
    sevExistingTags :: Text,
    sevScheduleStartDate :: Text, -- "YYYY-MM-DD" or ""
    sevScheduleMinDate :: Text, -- "YYYY-MM-DD" lower bound for the date picker, or ""

    -- | Currently-active templates shown as read-only when a pending schedule exists.
    sevCurrentScheduleTemplates :: [ShowSchedule.ScheduleTemplate Result],
    -- | Pending (future) templates shown as read-only preview alongside current.
    sevPendingScheduleTemplates :: [ShowSchedule.ScheduleTemplate Result]
  }

-- | Business logic: fetch show and staff data.
action ::
  UserMetadata.Model ->
  Slug ->
  ExceptT HandlerError AppM ShowEditViewData
action userMetadata slug = do
  -- 1. Get storage backend
  backend <- asks getter

  -- 2. Check if staff for conditional rendering
  let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole

  -- 3. Fetch the show to edit
  showModel <- fetchShowOrNotFound slug

  -- 4. Fetch existing tags for this show
  existingTagsResult <- execQuery (Shows.getTagsForShow showModel.id)
  let existingTags = case existingTagsResult of
        Left _ -> ""
        Right tags -> Text.intercalate ", " $ map ShowTags.stName tags

  -- 5. Fetch staff-only data (schedules, hosts) if user is staff
  (schedulesJson, eligibleHosts, currentHostIds, scheduleStartDate, scheduleMinDate, currentTemplates, pendingTemplates') <-
    fromRightM throwDatabaseError $
      lift $
        bool (pure (Right ("[]", [], Set.empty, "", "", [], []))) (fetchStaffData showModel.id) isStaff

  pure
    ShowEditViewData
      { sevUserMetadata = userMetadata,
        sevShowModel = showModel,
        sevBackend = backend,
        sevIsStaff = isStaff,
        sevSchedulesJson = schedulesJson,
        sevEligibleHosts = eligibleHosts,
        sevCurrentHostIds = currentHostIds,
        sevExistingTags = existingTags,
        sevScheduleStartDate = scheduleStartDate,
        sevScheduleMinDate = scheduleMinDate,
        sevCurrentScheduleTemplates = currentTemplates,
        sevPendingScheduleTemplates = pendingTemplates'
      }

handler ::
  Slug ->
  Satisfies 'UserMetadata.Host RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler slug host@(Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Show edit" apiLinks.rootGet $ do
    void $ authorizeShow host slug
    vd <- action auth.raMetadata slug
    let editTemplate = template vd.sevBackend vd.sevShowModel vd.sevUserMetadata vd.sevIsStaff vd.sevSchedulesJson vd.sevEligibleHosts vd.sevCurrentHostIds vd.sevExistingTags vd.sevScheduleStartDate vd.sevScheduleMinDate vd.sevCurrentScheduleTemplates vd.sevPendingScheduleTemplates
    lift $ renderDashboardTemplate hxRequest vd.sevUserMetadata (ForShow vd.sevShowModel) NavSettings Nothing Nothing editTemplate

-- | Fetch show by slug, throwing NotFound if not found
fetchShowOrNotFound ::
  Slug ->
  ExceptT HandlerError AppM Shows.Model
fetchShowOrNotFound slug =
  fromMaybeM (throwNotFound "Show") $
    fromRightM throwDatabaseError $
      execQuery (Shows.getShowBySlug slug)

-- | Fetch staff-only data for the edit form (schedules and hosts).
--
-- When a pending (future) schedule exists, the form is populated with the pending
-- schedule and the current active schedule is returned separately for read-only display.
-- When no pending schedule exists, the form shows the current active schedule.
fetchStaffData ::
  Shows.Id ->
  AppM (Either HSQL.Pool.UsageError (Text, [UserMetadata.UserWithMetadata], Set User.Id, Text, Text, [ShowSchedule.ScheduleTemplate Result], [ShowSchedule.ScheduleTemplate Result]))
fetchStaffData showId = do
  -- Today in Pacific, computed outside the DB transaction. Used both as the date
  -- picker's lower bound and to clamp the pre-filled start date to no earlier than
  -- today (a future/pending schedule keeps its date, a past active date becomes today).
  today <- pacificDay <$> currentSystemTime
  let todayText = Text.pack (show today)
  runDBTransaction $ do
    activeTemplates <- TRX.statement () $ ShowSchedule.getActiveScheduleTemplatesForShow showId
    pendingTemplates <- TRX.statement () $ ShowSchedule.getPendingScheduleTemplatesForShow showId
    eligibleHosts <- TRX.statement () $ UserMetadata.getAllUsersWithPagination 1000 0
    currentHostIds <- TRX.statement () $ ShowHost.getShowHosts showId

    -- If pending templates exist, populate form with them and show active as read-only.
    -- Otherwise, populate form with the active templates.
    let (formTemplates, currentForDisplay) = case pendingTemplates of
          [] -> (activeTemplates, [])
          _ -> (pendingTemplates, activeTemplates)

    let hasPending = not (null pendingTemplates)
    startDate <- case formTemplates of
      [] -> pure todayText
      (t : _) -> do
        -- For pending templates, use unfiltered query since their validity hasn't started yet.
        -- For active templates, use the active-only query.
        validities <-
          TRX.statement () $
            if hasPending
              then ShowSchedule.getValidityPeriodsForTemplate t.stId
              else ShowSchedule.getActiveValidityPeriodsForTemplate t.stId
        pure $ case validities of
          [] -> todayText
          vs -> Text.pack $ show $ max today $ minimum $ map (.stvEffectiveFrom) vs

    -- A show holds one slot, enforced by one_active_slot_per_show. Take the head so a
    -- database that predates the constraint still renders rather than failing.
    pure (schedulesToEditorJson (listToMaybe formTemplates), eligibleHosts, Set.fromList $ fmap (.shmUserId) currentHostIds, startDate, todayText, currentForDisplay, pendingTemplates)
