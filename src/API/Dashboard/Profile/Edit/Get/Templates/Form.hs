{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Profile.Edit.Get.Templates.Form where

--------------------------------------------------------------------------------

import API.Links (dashboardLinks)
import API.Types
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
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
profileEditPostUrl :: Links.URI
profileEditPostUrl = Links.linkURI dashboardLinks.profileEditPost

dashboardEpisodesRedirectUrl :: Links.URI
dashboardEpisodesRedirectUrl = Links.linkURI dashboardLinks.episodesRedirect

--------------------------------------------------------------------------------

template :: StorageBackend -> User.Model -> UserMetadata.Model -> Lucid.Html ()
template backend user metadata =
  renderForm config form
  where
    postUrl :: Text
    postUrl = [i|/#{profileEditPostUrl}|]

    avatarUrl :: Text
    avatarUrl = maybe "" (buildMediaUrl backend) metadata.mAvatarUrl

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
          Just _ -> currentFile avatarUrl
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

      -- Theme
      radioField "theme" do
        label "Theme"
        hint "Choose your color palette"
        themeOption metadata "Default" "Default" "KPBJ brutalist style with high contrast" UserMetadata.DefaultTheme
        themeOption metadata "Solarized" "Solarized" "Precision colors designed for readability" UserMetadata.SolarizedTheme
        themeOption metadata "Gruvbox" "Gruvbox" "Retro groove with warm, earthy tones" UserMetadata.GruvboxTheme
        themeOption metadata "Dracula" "Dracula" "Dark theme with vibrant purple accents" UserMetadata.DraculaTheme
        themeOption metadata "Nord" "Nord" "Arctic-inspired calm blue palette" UserMetadata.NordTheme

      -- Buttons
      cancelButton [i|/#{dashboardEpisodesRedirectUrl}|] "CANCEL"
      submitButton "SAVE CHANGES"

-- | Helper to add a color scheme option, selecting it if it matches current setting
colorSchemeOption :: UserMetadata.Model -> Text -> Text -> Text -> UserMetadata.ColorScheme -> FieldBuilder
colorSchemeOption metadata val lbl desc scheme =
  if metadata.mColorScheme == scheme
    then addOptionSelectedWithDesc val lbl desc
    else addOptionWithDesc val lbl desc

-- | Helper to add a theme option, selecting it if it matches current setting
themeOption :: UserMetadata.Model -> Text -> Text -> Text -> UserMetadata.ThemeName -> FieldBuilder
themeOption metadata val lbl desc themeName =
  if metadata.mTheme == themeName
    then addOptionSelectedWithDesc val lbl desc
    else addOptionWithDesc val lbl desc

-- | Role display field (read-only)
roleField :: UserMetadata.Model -> Lucid.Html ()
roleField metadata =
  Lucid.div_ $ do
    Lucid.label_ [class_ $ base ["block", T.fontBold, T.mb2]] "Role"
    Lucid.div_ [class_ $ base [T.p3, "border", T.borderGray600, T.bgGray900]] $
      Lucid.span_ [class_ $ base ["inline-flex", "items-center", T.px3, "py-1", T.textSm, T.fontBold, T.bgGray800, T.textWhite]] $
        Lucid.toHtml $
          show metadata.mUserRole
    Lucid.p_ [class_ $ base [T.textSm, "text-gray-500 dark:text-gray-400", "mt-1"]] "Role can only be changed by administrators"
