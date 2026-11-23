{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Invitations.Id.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardInvitationsLinks)
import API.Types
import Component.ScheduleEditor (ScheduleEditorData, renderScheduleEditor)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Lucid qualified
import Lucid.Form.Builder
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardInvitationsGetUrl :: Links.URI
dashboardInvitationsGetUrl = Links.linkURI dashboardInvitationsLinks.list

dashboardInvitationsEditPostUrl :: HostInvitation.Id -> Links.URI
dashboardInvitationsEditPostUrl invitationId = Links.linkURI $ dashboardInvitationsLinks.editPost invitationId

--------------------------------------------------------------------------------

-- | Edit invitation form template using FormBuilder.
--
-- It has the same sections as the new invitation form.
template ::
  HostInvitation.Id ->
  -- | The current recipient
  EmailAddress ->
  -- | The current schedule and start date
  ScheduleEditorData ->
  Lucid.Html ()
template invitationId recipientEmail editorData =
  renderForm config form
  where
    postUrl :: Text
    postUrl = [i|/#{dashboardInvitationsEditPostUrl invitationId}|]

    backUrl :: Text
    backUrl = [i|/#{dashboardInvitationsGetUrl}|]

    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = postUrl,
          fcMethod = "post",
          fcHtmxTarget = Just "#main-content",
          fcFooterLook = DashboardFooter
        }

    form :: FormBuilder
    form = do
      formTitle "EDIT HOST INVITATION"
      formSubtitle "The invitation link stays the same. No email is sent."

      -- Recipient Section
      section "RECIPIENT" $ do
        textField "recipient_email" $ do
          label "Recipient Email"
          placeholder "host@example.com"
          hint "Must not match an existing KPBJ account. Use Resend in the list to email the link to a new address."
          required
          value (display recipientEmail)

      -- Schedule Section
      section "SCHEDULE" $ do
        plain $ renderScheduleEditor editorData

      cancelButton backUrl "CANCEL"
      submitButton "SAVE INVITATION"
