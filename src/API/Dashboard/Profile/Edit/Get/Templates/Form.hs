{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Profile.Edit.Get.Templates.Form where

--------------------------------------------------------------------------------

import API.Links (apiLinks, dashboardLinks)
import API.Types
import Component.Form.V2
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as T
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
profileEditPostUrl :: Links.URI
profileEditPostUrl = Links.linkURI dashboardLinks.profileEditPost

postUrl :: Text
postUrl = [i|/#{profileEditPostUrl}|]

dashboardEpisodesRedirectUrl :: Links.URI
dashboardEpisodesRedirectUrl = Links.linkURI dashboardLinks.episodesRedirect

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

--------------------------------------------------------------------------------

template :: User.Model -> UserMetadata.Model -> Lucid.Html ()
template user metadata =
  renderForm config form
  where
    config =
      defaultFormConfig
        { fcAction = postUrl,
          fcMethod = "post",
          fcHtmxTarget = Just "#main-content",
          fcHtmxSwap = Just "innerHTML"
        }

    form = do
      formTitle "EDIT PROFILE"
      formSubtitle "Update your profile information"
      -- Email (read-only)
      textField "email" do
        label "Email"
        value (display user.mEmail)
        disabled
        hint "Email cannot be changed"

      -- Display Name
      textField "display_name" do
        label "Display Name"
        value (display metadata.mDisplayName)
        placeholder "Your display name"
        hint "This name will be shown publicly on the site"
        required

      -- Full Name
      textField "full_name" do
        label "Full Name"
        value (display metadata.mFullName)
        placeholder "Your full name"
        required

      -- Avatar Upload
      imageField "avatar" do
        label "Avatar Image"
        maxSize 10
        aspectRatio (1, 1)
        case metadata.mAvatarUrl of
          Just url -> currentFile [i|/#{mediaGetUrl}/#{url}|]
          Nothing -> pure ()

      -- Role (read-only)
      plain (roleField metadata)

      -- Color Scheme
      radioField "color_scheme" do
        label "Color Scheme"
        hint "Choose how the site appears to you"
        colorSchemeOption metadata "Automatic" "Automatic" "Follows system preference" UserMetadata.Automatic
        colorSchemeOption metadata "LightMode" "Light Mode" "Always use light theme" UserMetadata.LightMode
        colorSchemeOption metadata "DarkMode" "Dark Mode" "Always use dark theme" UserMetadata.DarkMode

      -- Buttons
      submitButton "SAVE CHANGES"
      cancelButton [i|/#{dashboardEpisodesRedirectUrl}|] "CANCEL"

-- | Helper to add a color scheme option, selecting it if it matches current setting
colorSchemeOption :: UserMetadata.Model -> Text -> Text -> Text -> UserMetadata.ColorScheme -> FieldBuilder
colorSchemeOption metadata val lbl desc scheme =
  if metadata.mColorScheme == scheme
    then addOptionSelectedWithDesc val lbl desc
    else addOptionWithDesc val lbl desc

-- | Role display field (read-only)
roleField :: UserMetadata.Model -> Lucid.Html ()
roleField metadata =
  Lucid.div_ $ do
    Lucid.label_ [class_ $ base ["block", T.fontBold, T.mb2]] "Role"
    Lucid.div_ [class_ $ base [T.p3, T.border2, "border-gray-300", T.bgGray100]] $
      Lucid.span_ [class_ $ base ["inline-flex", "items-center", T.px3, "py-1", T.textSm, T.fontBold, T.bgGray800, T.textWhite]] $
        Lucid.toHtml $
          show metadata.mUserRole
    Lucid.p_ [class_ $ base [T.textSm, "text-gray-500", "mt-1"]] "Role can only be changed by administrators"
