{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Users.Edit.Get.Templates.Page where

--------------------------------------------------------------------------------

import API.Links (apiLinks, dashboardUsersLinks)
import API.Types
import Component.Form.Builder
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as T
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | URL helpers
mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

--------------------------------------------------------------------------------

template :: User.Model -> UserMetadata.Model -> Lucid.Html ()
template user metadata = do
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
    avatarUrl = maybe "" (\path -> [i|/#{mediaGetUrl}/#{path}|]) metadata.mAvatarUrl

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

      section "" $ do
        -- Role Selector
        selectField "role" $ do
          hint "Changing user roles affects their permissions"
          required
          roleOption UserMetadata.User "User" metadata.mUserRole
          roleOption UserMetadata.Host "Host" metadata.mUserRole
          roleOption UserMetadata.Staff "Staff" metadata.mUserRole
          roleOption UserMetadata.Admin "Admin" metadata.mUserRole

        -- Info box
        plain roleInfoBox

      cancelButton backUrl "CANCEL"
      submitButton "SAVE CHANGES"

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
  Lucid.section_ [class_ $ base [T.bgGray800, T.textWhite, T.p6, T.mb8, T.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [T.text2xl, T.fontBold, T.mb2]] "EDIT USER"
        Lucid.div_ [class_ $ base ["text-gray-300", T.textSm]] $ do
          Lucid.strong_ "Editing: "
          Lucid.toHtml $ display user.mEmail
      Lucid.div_ $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{backUrl}|],
            hxGet_ [i|/#{backUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-300", "hover:text-blue-100", T.textSm, "underline"]
          ]
          "← BACK TO USER"

-- | Info box explaining role permissions
roleInfoBox :: Lucid.Html ()
roleInfoBox =
  Lucid.div_ [class_ $ base ["bg-blue-50", T.border2, "border-blue-200", T.p4]] $ do
    Lucid.p_ [class_ $ base [T.textSm, "text-blue-800"]] $ do
      Lucid.strong_ "Note: "
      "Changing user roles affects their permissions. "
      "Admin has full access, Staff can manage content, Host can manage shows, User has basic access."
