{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.StationIds.Id.Delete.Handler (handler, action) where

--------------------------------------------------------------------------------

import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwNotAuthorized, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (unless)
import Control.Monad.Trans.Except (ExceptT)
import Domain.Types.Cookie (Cookie (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + OOB banner.
handler ::
  StationIds.Id ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler stationIdId cookie =
  handleBannerErrors "Station ID delete" $ do
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to delete station IDs." userMetadata
    action user userMetadata stationIdId
    pure $ do
      mempty
      renderBanner Success "Station ID Deleted" "The station ID has been deleted successfully."

--------------------------------------------------------------------------------

-- | Business logic: ownership check, delete.
action ::
  User.Model ->
  UserMetadata.Model ->
  StationIds.Id ->
  ExceptT HandlerError AppM ()
action user userMetadata stationIdId = do
  -- 1. Fetch station ID
  stationId <- fetchStationId stationIdId

  -- 2. Check authorization: must be creator OR staff/admin
  let isCreator = stationId.simCreatorId == user.mId
      isStaffOrAdmin = UserMetadata.isStaffOrHigher userMetadata.mUserRole
      isAuthorized = isCreator || isStaffOrAdmin

  unless isAuthorized $
    throwNotAuthorized "You can only delete station IDs you created." (Just userMetadata.mUserRole)

  -- 3. Delete the station ID
  deleteStationId stationId

--------------------------------------------------------------------------------
-- Helpers

fetchStationId ::
  StationIds.Id ->
  ExceptT HandlerError AppM StationIds.Model
fetchStationId stationIdId =
  fromMaybeM (throwNotFound "Station ID") $
    fromRightM throwDatabaseError $
      execQuery (StationIds.getStationIdById stationIdId)

deleteStationId ::
  StationIds.Model ->
  ExceptT HandlerError AppM ()
deleteStationId stationId = do
  result <- execQuery (StationIds.deleteStationId stationId.simId)
  case result of
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Station ID"
    Right (Just _) ->
      Log.logInfo "Station ID deleted successfully" stationId.simId
