{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Users.Edit.Post.Handler where

--------------------------------------------------------------------------------

import API.Links (dashboardUsersLinks)
import API.Types
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError, throwNotFound, throwValidationError)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.DisplayName qualified as DisplayName
import Domain.Types.FileUpload qualified
import Domain.Types.FullName (FullName)
import Domain.Types.FullName qualified as FullName
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload qualified as FileUpload
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (Input (..), Mem, MultipartData (..), lookupFile)

--------------------------------------------------------------------------------

-- | URL for user detail page (redirect target)
userDetailUrl :: User.Id -> Text
userDetailUrl userId =
  let uri = Links.linkURI $ dashboardUsersLinks.detail userId
   in [i|/#{uri}|]

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  User.Id ->
  Maybe Cookie ->
  MultipartData Mem ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer targetUserId cookie multipartData =
  handleRedirectErrors "User edit" (dashboardUsersLinks.detail targetUserId) $ do
    -- Require admin authentication
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "You do not have permission to edit users." userMetadata

    -- Parse form fields
    let formInputs = inputs multipartData

    (newDisplayName, newFullName, newRole) <- case extractFormFields formInputs of
      Left errorMsg -> throwValidationError errorMsg
      Right result -> pure result

    -- Handle avatar upload if provided
    maybeAvatarPath <- case lookupFile "avatar" multipartData of
      Left _ -> pure Nothing
      Right avatarFile -> do
        uploadResult <- FileUpload.uploadUserAvatar (display targetUserId) avatarFile
        case uploadResult of
          Left _err -> throwValidationError "Failed to upload avatar image"
          Right Nothing -> pure Nothing
          Right (Just result) ->
            let storagePath = Domain.Types.FileUpload.uploadResultStoragePath result
             in pure (Just (FileUpload.stripStorageRoot storagePath))

    -- Update user metadata and role in transaction
    updateResult <- execTransactionSpan $ do
      -- Get current metadata
      maybeMetadata <- HT.statement () (UserMetadata.getUserMetadata targetUserId)
      case maybeMetadata of
        Nothing -> pure Nothing
        Just currentMetadata -> do
          -- Determine final avatar URL (use new upload or keep existing)
          let finalAvatarUrl = maybeAvatarPath <|> currentMetadata.mAvatarUrl

          -- Update metadata fields (color scheme is not editable by admins, users set it themselves)
          let metadataUpdate =
                UserMetadata.Update
                  { UserMetadata.uDisplayName = Just newDisplayName,
                    UserMetadata.uFullName = Just newFullName,
                    UserMetadata.uAvatarUrl = Just finalAvatarUrl,
                    UserMetadata.uColorScheme = Nothing
                  }
          _ <- HT.statement () (UserMetadata.updateUserMetadata currentMetadata.mId metadataUpdate)

          -- Update role
          _ <- HT.statement () (UserMetadata.updateUserRole targetUserId newRole)

          pure $ Just ()

    case updateResult of
      Left err -> throwDatabaseError err
      Right Nothing -> throwNotFound "User metadata"
      Right (Just ()) -> do
        Log.logInfo "User updated successfully" ()
        let detailUrl = userDetailUrl targetUserId
            banner = BannerParams Success "User Updated" "The user's information has been updated."
            redirectUrl = buildRedirectUrl detailUrl banner
        pure $ Servant.addHeader redirectUrl (redirectWithBanner detailUrl banner)

extractFormFields :: [Input] -> Either Text (DisplayName, FullName, UserMetadata.UserRole)
extractFormFields inputs = do
  -- Convert Input to (Text, Text) for lookup
  let inputPairs = [(iName input, iValue input) | input <- inputs]

  -- Extract text fields
  displayNameText <- maybe (Left "Missing display_name") Right $ lookup "display_name" inputPairs
  fullNameText <- maybe (Left "Missing full_name") Right $ lookup "full_name" inputPairs
  roleText <- maybe (Left "Missing role") Right $ lookup "role" inputPairs

  -- Parse display name
  displayName <- case DisplayName.mkDisplayName displayNameText of
    Nothing -> Left "Invalid display name"
    Just dn -> Right dn

  -- Parse full name
  fullName <- case FullName.mkFullName fullNameText of
    Nothing -> Left "Invalid full name"
    Just fn -> Right fn

  -- Parse role
  role <- case roleText of
    "User" -> Right UserMetadata.User
    "Host" -> Right UserMetadata.Host
    "Staff" -> Right UserMetadata.Staff
    "Admin" -> Right UserMetadata.Admin
    _ -> Left "Invalid role"

  pure (displayName, fullName, role)
