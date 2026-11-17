{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Invitations.Id.Edit.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.Id.Edit.Get.Templates.Form (template)
import API.Dashboard.Shows.Slug.Edit.Post.Handler (ParsedScheduleSlot (..))
import API.Invite.Token.Post.Handler (parseInvitationSchedules)
import API.Links (dashboardInvitationsLinks)
import API.Types (DashboardInvitationsRoutes (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.ScheduleEditor (ScheduleEditorData (..), slotToEditorJson)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, showGregorian)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Timezone (pacificDay)
import Effects.Clock (currentSystemTime)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/invitations/:invitationId/edit.
--
-- Shows the edit form for a pending invitation, with its recipient, its
-- schedule, and its start date filled in.
handler ::
  HostInvitation.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler invitationId cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Edit invitation form" dashboardInvitationsLinks.list $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can edit invitations." userMetadata

    inv <- fromRightM throwDatabaseError $ execQuery (HostInvitation.getById invitationId)
    invitation <- case inv of
      Nothing -> throwNotFound "Invitation"
      Just m
        | m.hiStatus /= HostInvitation.Pending ->
            throwValidationError "Only pending invitations can be edited."
        | otherwise -> pure m

    slot <- either throwValidationError pure (parseInvitationSchedules invitation.hiScheduleData)
    today <- pacificDay <$> lift currentSystemTime
    let editorData =
          ScheduleEditorData
            { sedExistingJson = maybe "null" slotEditorJson slot,
              sedStartDate = dateText invitation.hiStartsOn,
              -- A stored start date can be in the past. The picker must accept
              -- it, or the browser refuses to submit the form.
              sedMinDate = dateText (min today invitation.hiStartsOn)
            }
    lift $
      renderDashboardTemplate hxRequest userMetadata NoShow NavInvitations Nothing Nothing $
        template invitation.hiId invitation.hiRecipientEmail editorData

--------------------------------------------------------------------------------

-- | The stored slot in the JSON format that the schedule editor reads.
slotEditorJson :: ParsedScheduleSlot -> Text
slotEditorJson slot =
  slotToEditorJson (pssRecurrence slot) (pssStart slot) (pssEnd slot) (pssReplayStartTime slot)

-- | A date in the @YYYY-MM-DD@ format of the date picker.
dateText :: Day -> Text
dateText = Text.pack . showGregorian
