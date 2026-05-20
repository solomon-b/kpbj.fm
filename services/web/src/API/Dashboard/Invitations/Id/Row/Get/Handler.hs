{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Invitations.Id.Row.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.Get.Templates.Page (renderInvitationRow)
import App.BaseUrl (baseUrl)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Control.Monad.Trans (lift)
import Data.List qualified as List
import Domain.Types.Cookie (Cookie)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/invitations/:invitationId/row.
--
-- Returns the normal (non-edit) row fragment for the given invitation.
-- Used by HTMX to restore a row after entering edit mode.
handler ::
  HostInvitation.Id ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler invitationId cookie =
  handleBannerErrors "Render invitation row" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can view invitations." userMetadata
    rows <- fromRightM throwDatabaseError $ execQuery HostInvitation.getAllWithCreator
    case List.find (\r -> r.mwcId == invitationId) rows of
      Nothing -> throwNotFound "Invitation"
      Just r -> do
        appBaseUrl <- lift baseUrl
        pure (renderInvitationRow appBaseUrl r)
