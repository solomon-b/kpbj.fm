{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Profile.Edit.Post.Handler where

--------------------------------------------------------------------------------

import API.Links (dashboardLinks, rootLink)
import API.Types (DashboardRoutes (..))
import Amazonka qualified as AWS
import App.Handler.Combinators (requireAuth)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError, throwNotFound, throwUserSuspended, throwValidationError)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Has (Has, getter)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.DisplayName qualified as DisplayName
import Domain.Types.FileUpload qualified
import Domain.Types.FullName (FullName)
import Domain.Types.FullName qualified as FullName
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.StorageBackend (StorageBackend)
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
import Servant.Multipart (Input (..), Mem, MultipartData (..), lookupFile, lookupInput)

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadMask m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env,
    Has StorageBackend env,
    Has (Maybe AWS.Env) env
  ) =>
  Tracer ->
  Maybe Cookie ->
  Maybe HxRequest ->
  MultipartData Mem ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer cookie (foldHxReq -> _hxRequest) multipartData =
  handleRedirectErrors "Profile update" dashboardLinks.profileEditGet $ do
    -- 1. Require authentication
    (user, userMetadata) <- requireAuth cookie

    -- 2. Check not suspended
    when (UserMetadata.isSuspended userMetadata) throwUserSuspended

    -- 3. Parse and validate form fields
    (newDisplayName, newFullName, newColorScheme) <- parseFormFields (inputs multipartData)

    -- 4. Handle avatar upload if provided
    (maybeAvatarPath, avatarClear) <- handleAvatarUpload user multipartData

    -- 5. Update user metadata
    updateUserProfile user maybeAvatarPath avatarClear newDisplayName newFullName newColorScheme

    -- 6. Success redirect
    Log.logInfo "Profile updated successfully" ()
    let detailUrl = rootLink dashboardLinks.profileEditGet
        banner = BannerParams Success "Profile Updated" "Your profile has been updated successfully."
        redirectUrl = buildRedirectUrl detailUrl banner
    pure $ Servant.addHeader redirectUrl (redirectWithBanner detailUrl banner)

--------------------------------------------------------------------------------
-- Helpers

parseFormFields ::
  (MonadThrow m) =>
  [Input] ->
  m (DisplayName, FullName, UserMetadata.ColorScheme)
parseFormFields formInputs =
  case extractFormFields formInputs of
    Left errorMsg -> throwValidationError errorMsg
    Right result -> pure result

handleAvatarUpload ::
  (MonadUnliftIO m, Log.MonadLog m, MonadThrow m, MonadMask m, MonadIO m, MonadReader env m, Has StorageBackend env, Has (Maybe AWS.Env) env) =>
  User.Model ->
  MultipartData Mem ->
  m (Maybe Text, Bool) -- (Maybe new path, should clear)
handleAvatarUpload user multipartData = do
  -- Check if user wants to clear the avatar
  let avatarClear = case lookupInput "avatar_clear" multipartData of
        Right "true" -> True
        _ -> False

  case lookupFile "avatar" multipartData of
    Left _ -> pure (Nothing, avatarClear)
    Right avatarFile -> do
      storageBackend <- asks getter
      mAwsEnv <- asks getter
      uploadResult <- FileUpload.uploadUserAvatar storageBackend mAwsEnv (display (User.mId user)) avatarFile
      case uploadResult of
        Left _err -> throwValidationError "Failed to upload avatar image"
        Right Nothing -> pure (Nothing, avatarClear)
        Right (Just result) ->
          pure (Just $ Text.pack $ Domain.Types.FileUpload.uploadResultStoragePath result, False)

updateUserProfile ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m,
    MonadThrow m
  ) =>
  User.Model ->
  Maybe Text ->
  Bool -> -- avatarClear: if True and no new avatar, clear existing
  DisplayName ->
  FullName ->
  UserMetadata.ColorScheme ->
  m ()
updateUserProfile user maybeAvatarPath avatarClear newDisplayName newFullName newColorScheme = do
  updateResult <- execTransactionSpan $ do
    maybeMetadata <- HT.statement () (UserMetadata.getUserMetadata (User.mId user))
    case maybeMetadata of
      Nothing -> pure Nothing
      Just currentMetadata -> do
        -- Determine final avatar URL: new upload > explicit clear > keep existing
        let finalAvatarUrl = case (maybeAvatarPath, avatarClear) of
              (Just path, _) -> Just path -- New avatar uploaded
              (Nothing, True) -> Nothing -- User explicitly cleared
              (Nothing, False) -> currentMetadata.mAvatarUrl -- Keep existing
            metadataUpdate =
              UserMetadata.Update
                { UserMetadata.uDisplayName = Just newDisplayName,
                  UserMetadata.uFullName = Just newFullName,
                  UserMetadata.uAvatarUrl = Just finalAvatarUrl,
                  UserMetadata.uColorScheme = Just newColorScheme
                }
        _ <- HT.statement () (UserMetadata.updateUserMetadata currentMetadata.mId metadataUpdate)
        pure $ Just ()

  case updateResult of
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "User metadata"
    Right (Just ()) -> pure ()

extractFormFields :: [Input] -> Either Text (DisplayName, FullName, UserMetadata.ColorScheme)
extractFormFields formInputs = do
  -- Convert Input to (Text, Text) for lookup
  let inputPairs = [(iName input, iValue input) | input <- formInputs]

  -- Extract text fields
  displayNameText <- maybe (Left "Missing display_name") Right $ lookup "display_name" inputPairs
  fullNameText <- maybe (Left "Missing full_name") Right $ lookup "full_name" inputPairs
  colorSchemeText <- maybe (Left "Missing color_scheme") Right $ lookup "color_scheme" inputPairs

  -- Parse display name
  displayName <- case DisplayName.mkDisplayName displayNameText of
    Nothing -> Left "Invalid display name"
    Just dn -> Right dn

  -- Parse full name
  fullName <- case FullName.mkFullName fullNameText of
    Nothing -> Left "Invalid full name"
    Just fn -> Right fn

  -- Parse color scheme
  colorScheme <- case colorSchemeText of
    "Automatic" -> Right UserMetadata.Automatic
    "LightMode" -> Right UserMetadata.LightMode
    "DarkMode" -> Right UserMetadata.DarkMode
    _ -> Left "Invalid color scheme"

  pure (displayName, fullName, colorScheme)
