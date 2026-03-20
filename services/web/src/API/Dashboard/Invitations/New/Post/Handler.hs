{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Invitations.New.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Invitations.New.Post.Route (NewInvitationForm (..))
import API.Links (dashboardInvitationsLinks, inviteLinks, rootLink)
import API.Types
import App.BaseUrl (baseUrl)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson qualified as Aeson
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID.V4
import Domain.Types.Cookie (Cookie)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Servant qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardInvitationsGetUrl :: Text
dashboardInvitationsGetUrl =
  let listUrl = Links.linkURI dashboardInvitationsLinks.list
   in [i|/#{listUrl}|]

--------------------------------------------------------------------------------

-- | Handler for POST /dashboard/invitations/new.
handler ::
  Maybe Cookie ->
  NewInvitationForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler cookie form =
  handleRedirectErrors "Invitation creation" dashboardInvitationsLinks.newGet $ do
    -- 1. Authenticate and require staff role
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can create invitations." userMetadata

    -- 2. Validate that schedulesJson is non-empty valid JSON
    scheduleData <- validateSchedulesJson (nifSchedulesJson form)

    -- 3. Generate a token (UUID v4 with hyphens removed)
    token <- liftIO generateToken

    -- 4. Insert into host_invitations
    let insertData =
          HostInvitation.Insert
            { HostInvitation.iToken = token,
              HostInvitation.iScheduleData = scheduleData,
              HostInvitation.iCreatedBy = userMetadata.mUserId
            }

    insertResult <- execQuery (HostInvitation.insert insertData)
    case insertResult of
      Left dbErr -> throwDatabaseError dbErr
      Right Nothing -> throwHandlerFailure "Failed to create invitation."
      Right (Just _invitationId) -> pure ()

    -- 5. Build the invite URL for the flash message
    appBaseUrl <- lift baseUrl
    let inviteUrl = appBaseUrl <> inviteLinkPath token

    Log.logInfo "Created host invitation" (HostInvitation.unToken token)

    -- 6. Return redirect to invitation list with success flash
    let flash = FlashMessage Success "Invitation Created" [i|Invite link: #{inviteUrl}|]
    pure $ Servant.addHeader dashboardInvitationsGetUrl $ Servant.addHeader (flashCookie (Just flash)) Servant.NoContent


--------------------------------------------------------------------------------

-- | Generate a cryptographically secure token for host invitations.
--
-- Uses UUID v4 (random) with hyphens removed for a 32-character hex string.
generateToken :: IO HostInvitation.Token
generateToken = do
  uuid <- UUID.V4.nextRandom
  let tokenText = Text.filter (/= '-') $ UUID.toText uuid
  pure $ HostInvitation.Token tokenText


-- | Validate that the schedules JSON is non-empty and valid.
--
-- Returns the parsed Aeson 'Value' for storage in the database.
validateSchedulesJson ::
  Text ->
  ExceptT HandlerError AppM Aeson.Value
validateSchedulesJson schedulesJson
  | Text.null (Text.strip schedulesJson) =
      throwValidationError "Schedule is required. Please add at least one time slot."
  | schedulesJson == "[]" =
      throwValidationError "Schedule is required. Please add at least one time slot."
  | otherwise =
      case Aeson.eitherDecodeStrict (Text.encodeUtf8 schedulesJson) of
        Left err ->
          throwValidationError ("Invalid schedule data: " <> Text.pack err)
        Right val -> pure val


-- | Build the path portion of the invite link for a given token.
inviteLinkPath :: HostInvitation.Token -> Text
inviteLinkPath token =
  rootLink $ onboardGet inviteLinks token
