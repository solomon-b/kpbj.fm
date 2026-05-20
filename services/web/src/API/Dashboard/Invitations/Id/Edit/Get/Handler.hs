{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Invitations.Id.Edit.Get.Handler (handler, renderEditRow) where

--------------------------------------------------------------------------------

import API.Links (dashboardInvitationsLinks)
import API.Types (DashboardInvitationsRoutes (..))
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie)
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Total number of columns rendered by 'renderInvitationRow'.
--
-- Schedule, Recipient, Status, Created By, Expires, Actions = 6.
rowColumnCount :: Int
rowColumnCount = 6

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/invitations/:invitationId/edit.
--
-- Returns an inline edit-form row fragment for the given pending invitation.
handler ::
  HostInvitation.Id ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler invitationId cookie =
  handleBannerErrors "Edit invitation form" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can edit invitations." userMetadata
    inv <- fromRightM throwDatabaseError $ execQuery (HostInvitation.getById invitationId)
    case inv of
      Nothing -> throwNotFound "Invitation"
      Just m
        | m.hiStatus /= HostInvitation.Pending ->
            throwValidationError "Only pending invitations can be edited."
        | otherwise ->
            pure (renderEditRow m.hiId m.hiRecipientEmail)

--------------------------------------------------------------------------------

-- | Render an inline edit-form @\<tr\>@ for an invitation.
--
-- Replaces the normal row in-place via HTMX. Save targets the edit POST;
-- Cancel targets the row-get fragment.
renderEditRow ::
  HostInvitation.Id ->
  EmailAddress ->
  Lucid.Html ()
renderEditRow invitationId currentEmail =
  Lucid.tr_
    [ Lucid.id_ rowId,
      class_ $ base ["border-b-2", Tokens.borderMuted]
    ]
    $ Lucid.td_
      [ Lucid.colspan_ (display rowColumnCount),
        class_ $ base [Tokens.p4]
      ]
    $ Lucid.div_ [class_ $ base ["flex", "gap-2", "items-center"]]
    $ do
      Lucid.input_
        [ Lucid.type_ "email",
          Lucid.name_ "recipient_email",
          Lucid.value_ (display currentEmail),
          Lucid.required_ "required",
          class_ $ base ["flex-1", Tokens.px3, "py-1", Tokens.textSm, Tokens.bgAlt, Tokens.fgPrimary, "border", Tokens.borderMuted]
        ]
      Lucid.button_
        [ hxPost_ editPostUrl,
          hxInclude_ saveIncludeSelector,
          hxTarget_ rowSelector,
          hxSwap_ "outerHTML",
          class_ $ base [Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, Tokens.successBg, Tokens.successText, "border", Tokens.borderMuted, "hover:opacity-80"]
        ]
        "SAVE"
      Lucid.button_
        [ hxGet_ rowGetUrl,
          hxTarget_ rowSelector,
          hxSwap_ "outerHTML",
          class_ $ base [Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, Tokens.bgAlt, Tokens.fgPrimary, "border", Tokens.borderMuted, "hover:opacity-80"]
        ]
        "CANCEL"
  where
    invitationIdText :: Text
    invitationIdText = display invitationId

    rowId :: Text
    rowId = "invitation-row-" <> invitationIdText

    rowSelector :: Text
    rowSelector = "#" <> rowId

    saveIncludeSelector :: Text
    saveIncludeSelector = rowSelector <> " input[name='recipient_email']"

    editPostUrl :: Text
    editPostUrl =
      let link = Links.linkURI $ dashboardInvitationsLinks.editPost invitationId
       in [i|/#{link}|]

    rowGetUrl :: Text
    rowGetUrl =
      let link = Links.linkURI $ dashboardInvitationsLinks.rowGet invitationId
       in [i|/#{link}|]
