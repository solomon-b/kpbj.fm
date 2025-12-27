{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Profile.Edit.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Profile.Edit.Get.Templates.Form qualified as ProfileForm
import API.Links (dashboardLinks)
import API.Types (DashboardRoutes (..))
import App.Common (getUserInfo, renderDashboardTemplate)
import Component.Banner (BannerType (..), renderBanner)
import Component.DashboardFrame (DashboardNav (..))
import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.ByteString.Lazy qualified as BSL
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Maybe (listToMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.DisplayName qualified as DisplayName
import Domain.Types.FileUpload qualified
import Domain.Types.FullName (FullName)
import Domain.Types.FullName qualified as FullName
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload qualified as FileUpload
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData (..), Input (..), Mem, MultipartData (..), lookupFile)

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
  Maybe Cookie ->
  Maybe HxRequest ->
  MultipartData Mem ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
handler _tracer cookie (foldHxReq -> hxRequest) multipartData = do
  getUserInfo cookie >>= \case
    Nothing -> pure $ Servant.noHeader (renderBanner Error "Login Required" "You must be logged in to update your profile.")
    Just (_user, userMetadata)
      | isSuspended userMetadata ->
          pure $ Servant.noHeader (renderBanner Error "Account Suspended" "Your account is suspended. You cannot update your profile.")
    Just (user, userMetadata) -> do
      -- Parse form fields
      let formInputs = inputs multipartData

      case extractFormFields formInputs of
        Left errorMsg ->
          pure $ Servant.noHeader (renderBanner Error "Error Updating Profile" errorMsg)
        Right (newDisplayName, newFullName, newColorScheme) -> do
          -- Handle avatar upload if provided (skip if file is empty)
          avatarUploadResult <- case lookupFile "avatar" multipartData of
            Left _ -> pure (Right Nothing)
            Right avatarFile
              | BSL.null (fdPayload avatarFile) -> pure (Right Nothing) -- No file selected
              | otherwise -> do
                  uploadResult <- FileUpload.uploadUserAvatar (display (User.mId user)) avatarFile
                  case uploadResult of
                    Left _err -> pure (Left "Failed to upload avatar image")
                    Right result ->
                      let storagePath = Domain.Types.FileUpload.uploadResultStoragePath result
                       in pure (Right (Just (FileUpload.stripStorageRoot storagePath)))

          case avatarUploadResult of
            Left errorMsg ->
              pure $ Servant.noHeader (renderBanner Error "Error Updating Profile" errorMsg)
            Right maybeAvatarPath -> do
              -- Update user metadata in transaction
              updateResult <- execTransactionSpan $ do
                -- Get current metadata
                maybeMetadata <- HT.statement () (UserMetadata.getUserMetadata (User.mId user))
                case maybeMetadata of
                  Nothing -> pure Nothing
                  Just currentMetadata -> do
                    -- Determine final avatar URL (use new upload or keep existing)
                    let finalAvatarUrl = maybeAvatarPath <|> currentMetadata.mAvatarUrl

                    -- Update metadata fields
                    let metadataUpdate =
                          UserMetadata.Update
                            { UserMetadata.uDisplayName = Just newDisplayName,
                              UserMetadata.uFullName = Just newFullName,
                              UserMetadata.uAvatarUrl = Just finalAvatarUrl,
                              UserMetadata.uColorScheme = Just newColorScheme
                            }
                    _ <- HT.statement () (UserMetadata.updateUserMetadata currentMetadata.mId metadataUpdate)

                    pure $ Just ()

              case updateResult of
                Left err -> do
                  Log.logInfo "Failed to update profile" (show err)
                  pure $ Servant.noHeader (renderBanner Error "Error Updating Profile" "Failed to update profile. Please try again.")
                Right Nothing ->
                  pure $ Servant.noHeader (renderBanner Error "Error Updating Profile" "User metadata not found.")
                Right (Just ()) -> do
                  Log.logInfo "Profile updated successfully" ()
                  -- Fetch shows for sidebar
                  showsResult <-
                    if UserMetadata.isAdmin userMetadata.mUserRole
                      then execQuerySpan Shows.getAllActiveShows
                      else execQuerySpan (Shows.getShowsForUser (User.mId user))
                  let allShows = fromRight [] showsResult
                      selectedShow = listToMaybe allShows

                  -- Fetch updated user data
                  updatedMetadataResult <- execQuerySpan (UserMetadata.getUserMetadata (User.mId user))

                  case updatedMetadataResult of
                    Left _err ->
                      pure $ Servant.noHeader (renderBanner Error "Error Updating Profile" "Profile updated but failed to reload data.")
                    Right Nothing ->
                      pure $ Servant.noHeader (renderBanner Error "Error Updating Profile" "Profile not found after update.")
                    Right (Just updatedMetadata) -> do
                      let editUrl = Links.linkURI dashboardLinks.profileEditGet
                          pageContent = do
                            ProfileForm.template user updatedMetadata
                            renderBanner Success "Profile Updated" "Your profile has been updated successfully."
                      html <- renderDashboardTemplate hxRequest updatedMetadata allShows selectedShow NavSettings Nothing Nothing pageContent
                      pure $ Servant.addHeader [i|/#{editUrl}|] html

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
