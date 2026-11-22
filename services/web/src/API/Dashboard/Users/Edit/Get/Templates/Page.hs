{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Users.Edit.Get.Templates.Page where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Edit.Shared (CanChangeRole)
import API.Links (dashboardUsersLinks)
import API.Types
import Control.Monad (forM_)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as T
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Form.Builder
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | The edit form. The role field shows only with a 'CanChangeRole'.
template ::
  -- | The permission to change the role, for an Admin
  Maybe CanChangeRole ->
  StorageBackend ->
  User.Model ->
  UserMetadata.Model ->
  Lucid.Html ()
template mCanChangeRole backend user metadata = do
  renderFormHeader user metadata
  renderForm config form
  where
    userId :: User.Id
    userId = user.mId

    backUri :: Links.URI
    backUri = Links.linkURI $ dashboardUsersLinks.detail userId

    postUri :: Links.URI
    postUri = Links.linkURI $ dashboardUsersLinks.editPost userId

    backUrl :: Text
    backUrl = [i|/#{backUri}|]

    postUrl :: Text
    postUrl = [i|/#{postUri}|]

    avatarUrl :: Text
    avatarUrl = maybe "" (buildMediaUrl backend) metadata.mAvatarUrl

    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = postUrl,
          fcMethod = "post",
          fcHtmxTarget = Just "#main-content",
          fcHtmxSwap = Just "innerHTML"
        }

    form :: FormBuilder
    form = do
      -- Display Name
      textField "display_name" $ do
        label "Display Name"
        value (display metadata.mDisplayName)
        placeholder "Display name"
        required

      -- Full Name
      textField "full_name" $ do
        label "Full Name"
        value (display metadata.mFullName)
        placeholder "Full name"
        required

      -- Avatar Upload
      imageField "avatar" $ do
        label "Avatar Image"
        maxSize 10
        aspectRatio (1, 1)
        hint "Optional: Upload a new avatar image (JPEG, PNG, WebP, or GIF, max 10MB)"
        case metadata.mAvatarUrl of
          Just _ -> currentFile avatarUrl
          Nothing -> pure ()

      forM_ mCanChangeRole $ \canChangeRole ->
        roleSection canChangeRole metadata.mUserRole

      cancelButton backUrl "CANCEL"
      submitButton "SAVE CHANGES"

-- | The role selector. It needs the permission to change the role.
roleSection :: CanChangeRole -> UserMetadata.UserRole -> FormBuilder
roleSection _ currentRole =
  section "" $ do
    -- Role Selector
    selectField "role" $ do
      hint "Changing user roles affects their permissions"
      required
      roleOption UserMetadata.User "User" currentRole
      roleOption UserMetadata.Host "Host" currentRole
      roleOption UserMetadata.Staff "Staff" currentRole
      roleOption UserMetadata.Admin "Admin" currentRole

    -- Info box
    plain roleInfoBox

-- | Helper to add a role option, selecting it if it matches current role
roleOption :: UserMetadata.UserRole -> Text -> UserMetadata.UserRole -> FieldBuilder
roleOption role roleText currentRole =
  if role == currentRole
    then addOptionSelected roleText roleText
    else addOption roleText roleText

-- | Form header with title and back link
renderFormHeader :: User.Model -> UserMetadata.Model -> Lucid.Html ()
renderFormHeader user _metadata = do
  let backUrl = Links.linkURI $ dashboardUsersLinks.detail user.mId
  Lucid.section_ [class_ $ base [T.bgInverse, T.fgInverse, T.p6, T.mb8, T.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [T.text2xl, T.fontBold, T.mb2]] "EDIT USER"
        Lucid.div_ [class_ $ base [T.fgMuted, T.textSm]] $ do
          Lucid.strong_ "Editing: "
          Lucid.toHtml $ display user.mEmail
      Lucid.div_ $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{backUrl}|],
            hxGet_ [i|/#{backUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base [T.infoText, "hover:opacity-80", T.textSm, "underline"]
          ]
          "← BACK TO USER"

-- | Info box explaining role permissions
roleInfoBox :: Lucid.Html ()
roleInfoBox =
  Lucid.div_ [class_ $ base [T.infoBg, T.border2, T.infoBorder, T.p4]] $ do
    Lucid.p_ [class_ $ base [T.textSm, T.infoText]] $ do
      Lucid.strong_ "Note: "
      "Changing user roles affects their permissions. "
      "Admin has full access, Staff can manage content, Host can manage shows, User has basic access."
