{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Invitations.New.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardInvitationsLinks)
import API.Types
import Component.ScheduleEditor (ScheduleEditorData (..), renderScheduleEditor)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Lucid qualified
import Lucid.Form.Builder
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardInvitationsGetUrl :: Links.URI
dashboardInvitationsGetUrl = Links.linkURI dashboardInvitationsLinks.list

dashboardInvitationsNewPostUrl :: Links.URI
dashboardInvitationsNewPostUrl = Links.linkURI dashboardInvitationsLinks.newPost

--------------------------------------------------------------------------------

-- | New invitation form template using FormBuilder.
template :: Lucid.Html ()
template =
  renderForm config form
  where
    postUrl :: Text
    postUrl = [i|/#{dashboardInvitationsNewPostUrl}|]

    backUrl :: Text
    backUrl = [i|/#{dashboardInvitationsGetUrl}|]

    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = postUrl,
          fcMethod = "post",
          fcHtmxTarget = Just "#main-content"
        }

    form :: FormBuilder
    form = do
      formTitle "GENERATE HOST INVITATION"
      formSubtitle "Create a signup link for a new host."

      -- Recipient Section
      section "RECIPIENT" $ do
        textField "recipient_email" $ do
          label "Recipient Email"
          placeholder "host@example.com"
          hint "The invitation email will be sent here. Must not match an existing KPBJ account."
          required

      -- Schedule Section
      section "SCHEDULE" $ do
        plain $ renderScheduleEditor (ScheduleEditorData "[]" "")

      cancelButton backUrl "CANCEL"
      submitButton "GENERATE INVITATION LINK"
