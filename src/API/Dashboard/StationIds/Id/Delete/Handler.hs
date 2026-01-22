{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.StationIds.Id.Delete.Handler (handler) where

--------------------------------------------------------------------------------

import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (unless)
import Domain.Types.Cookie (Cookie (..))
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  StationIds.Id ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler _tracer stationIdId cookie =
  handleBannerErrors "Station ID delete" $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to delete station IDs." userMetadata

    -- 2. Fetch station ID
    stationId <- fetchStationId stationIdId

    -- 3. Check authorization: must be creator OR staff/admin
    let isCreator = stationId.simCreatorId == user.mId
        isStaffOrAdmin = UserMetadata.isStaffOrHigher userMetadata.mUserRole
        isAuthorized = isCreator || isStaffOrAdmin

    unless isAuthorized $
      throwNotAuthorized "You can only delete station IDs you created." (Just userMetadata.mUserRole)

    -- 4. Delete the station ID
    deleteStationId stationId

--------------------------------------------------------------------------------
-- Helpers

fetchStationId ::
  StationIds.Id ->
  AppM StationIds.Model
fetchStationId stationIdId =
  execQuerySpan (StationIds.getStationIdById stationIdId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Station ID"
    Right (Just stationId) -> pure stationId

deleteStationId ::
  StationIds.Model ->
  AppM (Lucid.Html ())
deleteStationId stationId =
  execQuerySpan (StationIds.deleteStationId stationId.simId) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Station ID"
    Right (Just _) -> do
      Log.logInfo "Station ID deleted successfully" stationId.simId
      -- Return empty (removes row) + OOB banner (Pattern C)
      pure $ do
        mempty -- Empty response removes the target element
        renderBanner Success "Station ID Deleted" "The station ID has been deleted successfully."
