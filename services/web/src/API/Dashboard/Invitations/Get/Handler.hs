{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Invitations.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardInvitationsLinks, rootLink)
import API.Types (DashboardInvitationsRoutes (..), Routes (..))
import App.BaseUrl qualified
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Lucid qualified
import Lucid.HTMX
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/invitations.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Invitation list" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can view invitations." userMetadata
    invitations <- fromRightM throwDatabaseError $ execQuery HostInvitation.getAllWithCreator
    appBaseUrl <- lift App.BaseUrl.baseUrl
    lift $
      renderDashboardTemplate
        hxRequest
        userMetadata
        []
        Nothing
        NavInvitations
        Nothing
        (Just actionButton)
        (template appBaseUrl invitations)

-- | Action button for creating a new invitation.
actionButton :: Lucid.Html ()
actionButton =
  Lucid.a_
    [ Lucid.href_ newInvitationUrl,
      hxGet_ newInvitationUrl,
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      class_ $ base [Tokens.bgAlt, Tokens.fgPrimary, Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, Tokens.hoverBg]
    ]
    "New Invitation"
  where
    newInvitationUrl = rootLink dashboardInvitationsLinks.newGet
