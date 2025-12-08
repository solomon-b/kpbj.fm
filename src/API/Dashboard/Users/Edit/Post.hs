{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Users.Edit.Post where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Detail.Get.Templates.Page qualified as DetailPage
import API.Links (dashboardUsersLinks)
import API.Types (DashboardUsersRoutes (..))
import App.Common (getUserInfo, renderDashboardTemplate)
import Component.Banner (BannerType (..), renderBanner)
import Component.DashboardFrame (DashboardNav (..))
import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
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
import Effects.Database.Tables.Episodes qualified as Episodes
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
import Servant.Multipart (Input (..), Mem, MultipartData (..), lookupFile)

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
  Maybe HxRequest ->
  MultipartData Mem ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
handler _tracer targetUserId cookie (foldHxReq -> hxRequest) multipartData = do
  getUserInfo cookie >>= \case
    Nothing -> pure $ Servant.noHeader (renderBanner Error "Login Required" "You must be logged in to edit users.")
    Just (_user, userMetadata)
      | not (UserMetadata.isAdmin userMetadata.mUserRole) || isSuspended userMetadata ->
          pure $ Servant.noHeader (renderBanner Error "Admin Access Required" "You do not have permission to edit users.")
    Just (user, userMetadata) -> do
      -- Parse form fields
      let formInputs = inputs multipartData

      case extractFormFields formInputs of
        Left errorMsg ->
          pure $ Servant.noHeader (renderBanner Error "Error Updating User" errorMsg)
        Right (newDisplayName, newFullName, newRole) -> do
          -- Handle avatar upload if provided
          avatarUploadResult <- case lookupFile "avatar" multipartData of
            Left _ -> pure (Right Nothing)
            Right avatarFile -> do
              uploadResult <- FileUpload.uploadUserAvatar (display targetUserId) avatarFile
              case uploadResult of
                Left _err -> pure (Left "Failed to upload avatar image")
                Right result ->
                  let storagePath = Domain.Types.FileUpload.uploadResultStoragePath result
                   in pure (Right (Just (FileUpload.stripStorageRoot storagePath)))

          case avatarUploadResult of
            Left errorMsg ->
              pure $ Servant.noHeader (renderBanner Error "Error Updating User" errorMsg)
            Right maybeAvatarPath -> do
              -- Update user metadata and role in transaction
              updateResult <- execTransactionSpan $ do
                -- Get current metadata
                maybeMetadata <- HT.statement () (UserMetadata.getUserMetadata targetUserId)
                case maybeMetadata of
                  Nothing -> pure Nothing
                  Just currentMetadata -> do
                    -- Determine final avatar URL (use new upload or keep existing)
                    let finalAvatarUrl = maybeAvatarPath <|> currentMetadata.mAvatarUrl

                    -- Update metadata fields
                    let metadataUpdate =
                          UserMetadata.ModelUpdate
                            { UserMetadata.muDisplayName = Just newDisplayName,
                              UserMetadata.muFullName = Just newFullName,
                              UserMetadata.muAvatarUrl = Just finalAvatarUrl
                            }
                    _ <- HT.statement () (UserMetadata.updateUserMetadata currentMetadata.mId metadataUpdate)

                    -- Update role
                    _ <- HT.statement () (UserMetadata.updateUserRole targetUserId newRole)

                    pure $ Just ()

              case updateResult of
                Left _err -> do
                  Log.logInfo "Failed to update user" ()
                  pure $ Servant.noHeader (renderBanner Error "Error Updating User" "Failed to update user. Please try again.")
                Right Nothing ->
                  pure $ Servant.noHeader (renderBanner Error "Error Updating User" "User metadata not found.")
                Right (Just ()) -> do
                  Log.logInfo "User updated successfully" ()
                  -- Fetch shows for sidebar (admins see all, staff see their assigned shows)
                  showsResult <-
                    if UserMetadata.isAdmin userMetadata.mUserRole
                      then execQuerySpan Shows.getAllActiveShows
                      else execQuerySpan (Shows.getShowsForUser (User.mId user))
                  let allShows = either (const []) id showsResult
                      selectedShow = listToMaybe allShows

                  -- Fetch updated user data for detail page
                  updatedUserData <- execTransactionSpan $ do
                    maybeUser <- HT.statement () (User.getUser targetUserId)
                    maybeMetadata <- HT.statement () (UserMetadata.getUserMetadata targetUserId)
                    shows' <- HT.statement () (Shows.getShowsForUser targetUserId)
                    episodes <- HT.statement () (Episodes.getEpisodesByUser targetUserId 10 0)
                    pure (maybeUser, maybeMetadata, shows', episodes)

                  case updatedUserData of
                    Left _err ->
                      pure $ Servant.noHeader (renderBanner Error "Error Updating User" "User updated but failed to load details.")
                    Right (Nothing, _, _, _) ->
                      pure $ Servant.noHeader (renderBanner Error "Error Updating User" "User not found after update.")
                    Right (_, Nothing, _, _) ->
                      pure $ Servant.noHeader (renderBanner Error "Error Updating User" "User metadata not found after update.")
                    Right (Just updatedUser, Just updatedMetadata, shows', episodes) -> do
                      let detailUrl = Links.linkURI $ dashboardUsersLinks.detail targetUserId
                          pageContent = do
                            DetailPage.template updatedUser updatedMetadata shows' episodes
                            renderBanner Success "User Updated" "The user's information has been updated."
                      html <- renderDashboardTemplate hxRequest userMetadata allShows selectedShow NavUsers Nothing Nothing pageContent
                      pure $ Servant.addHeader [i|/#{detailUrl}|] html

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
