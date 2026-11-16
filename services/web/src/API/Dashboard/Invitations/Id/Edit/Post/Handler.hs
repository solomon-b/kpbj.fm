{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Invitations.Id.Edit.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.Conflict (checkInvitationConflicts)
import API.Dashboard.Invitations.Id.Edit.Post.Route (EditInvitationForm (..))
import API.Dashboard.Invitations.New.Post.Handler (validateRecipientEmail, validateSchedulesJson)
import API.Dashboard.Shows.Slug.Edit.Post.Handler (parseScheduleStartDate)
import API.Links (dashboardInvitationsLinks)
import API.Types
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, showGregorian)
import Domain.Types.Cookie (Cookie)
import Domain.Types.Timezone (pacificDay)
import Effects.Clock (currentSystemTime)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Servant qualified
import Servant.Links qualified as Links
import Utils (fromRightM)

--------------------------------------------------------------------------------

dashboardInvitationsGetUrl :: Text
dashboardInvitationsGetUrl =
  let listUrl = Links.linkURI dashboardInvitationsLinks.list
   in [i|/#{listUrl}|]

--------------------------------------------------------------------------------

-- | Handler for POST /dashboard/invitations/:invitationId/edit.
--
-- Updates the recipient, the schedule, and the start date of a pending
-- invitation. Redirects to the invitation list with a success flash.
handler ::
  HostInvitation.Id ->
  Maybe Cookie ->
  EditInvitationForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler invitationId cookie form =
  handleRedirectErrors "Invitation update" (dashboardInvitationsLinks.editGet invitationId) $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can edit invitations." userMetadata

    action invitationId form

    let flash = FlashMessage Success "Invitation updated" "The invitation link stays the same. No email was sent."
    pure $
      Servant.addHeader dashboardInvitationsGetUrl $
        Servant.addHeader (flashCookie (Just flash)) Servant.NoContent

-- | Validate the form and update the invitation.
action ::
  HostInvitation.Id ->
  EditInvitationForm ->
  ExceptT HandlerError AppM ()
action invitationId form = do
  -- 1. Only a pending invitation can be edited.
  inv <- fromRightM throwDatabaseError $ execQuery (HostInvitation.getById invitationId)
  invitation <- case inv of
    Nothing -> throwNotFound "Invitation"
    Just m
      | m.hiStatus /= HostInvitation.Pending ->
          throwValidationError "Only pending invitations can be edited."
      | otherwise -> pure m

  -- 2. Validate the recipient and the schedule, as the new invitation form does.
  validatedEmail <- validateRecipientEmail (eifRecipientEmail form)
  (scheduleData, slot) <- validateSchedulesJson (eifSchedulesJson form)

  -- 3. Validate the start date.
  today <- pacificDay <$> lift currentSystemTime
  startsOn <- validateStartDate today invitation.hiStartsOn (eifScheduleStartDate form)

  -- 4. Reject a slot that a show holds, or that another live invitation holds.
  --    The claim starts the schedule on today if the start date has passed, so
  --    the check uses the same date.
  checkInvitationConflicts (Just invitationId) (Just slot) (max startsOn today)

  -- 5. Update only if the invitation is still pending.
  updated <-
    fromRightM throwDatabaseError $
      execQuery (HostInvitation.updatePending invitationId validatedEmail scheduleData startsOn)
  case updated of
    Nothing -> throwValidationError "Invitation not found or no longer pending."
    Just _ -> pure ()

--------------------------------------------------------------------------------

-- | Validate the submitted start date.
--
-- A stored start date can be in the past, because a host can claim the
-- invitation after that date. The form accepts the stored date with no change,
-- so staff can edit the slot only. A new date must be today or later.
validateStartDate ::
  -- | Today, as a Pacific date
  Day ->
  -- | The stored start date
  Day ->
  -- | The submitted date
  Text ->
  ExceptT HandlerError AppM Day
validateStartDate today stored submitted = case Text.strip submitted of
  "" -> throwValidationError "Schedule start date is required."
  dateText
    | dateText == Text.pack (showGregorian stored) -> pure stored
    | otherwise -> either throwValidationError pure (parseScheduleStartDate today dateText)
