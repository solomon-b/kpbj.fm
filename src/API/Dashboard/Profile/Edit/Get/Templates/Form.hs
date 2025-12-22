{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Profile.Edit.Get.Templates.Form where

--------------------------------------------------------------------------------

import API.Links (dashboardLinks)
import API.Types (DashboardRoutes (..))
import Component.Form.Builder
  ( FormBuilder (..),
    FormField (..),
    HtmxConfig (..),
    SelectOption (..),
    ValidationRules (..),
    buildValidatedForm,
    defaultFormStyles,
    emptyValidation,
  )
import Component.ImageFilePicker qualified as ImageFilePicker
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as T
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
profileEditPostUrl :: Links.URI
profileEditPostUrl = Links.linkURI dashboardLinks.profileEditPost

postUrl :: Text
postUrl = [i|/#{profileEditPostUrl}|]

dashboardEpisodesRedirectUrl :: Links.URI
dashboardEpisodesRedirectUrl = Links.linkURI dashboardLinks.episodesRedirect

--------------------------------------------------------------------------------

template :: User.Model -> UserMetadata.Model -> Lucid.Html ()
template user metadata = do
  buildValidatedForm
    FormBuilder
      { fbAction = postUrl,
        fbMethod = "post",
        fbHeader = Just pageHeader,
        fbFields = formFields user metadata,
        fbAdditionalContent = [submitButtons],
        fbStyles = defaultFormStyles,
        fbHtmx = Just HtmxConfig {hcTarget = "#main-content", hcSwap = Just "innerHTML"}
      }

-- | Page header section
pageHeader :: Lucid.Html ()
pageHeader =
  Lucid.section_ [class_ $ base [T.sectionBase]] $ do
    Lucid.h1_ [class_ $ base [T.heading2xl]] "EDIT PROFILE"
    Lucid.p_ [class_ $ base [T.textGray600, T.mt4]] "Update your profile information"

-- | All form fields
formFields :: User.Model -> UserMetadata.Model -> [FormField]
formFields user metadata =
  [ -- Email (read-only)
    PlainField {pfHtml = emailField user},
    -- Display Name
    ValidatedTextField
      { vfName = "display_name",
        vfLabel = "Display Name",
        vfInitialValue = Just (display metadata.mDisplayName),
        vfPlaceholder = Just "Your display name",
        vfHint = Just "This name will be shown publicly on the site",
        vfValidation = emptyValidation {vrRequired = True}
      },
    -- Full Name
    ValidatedTextField
      { vfName = "full_name",
        vfLabel = "Full Name",
        vfInitialValue = Just (display metadata.mFullName),
        vfPlaceholder = Just "Your full name",
        vfHint = Nothing,
        vfValidation = emptyValidation {vrRequired = True}
      },
    -- Avatar Upload
    PlainField {pfHtml = avatarField metadata},
    -- Role (read-only)
    PlainField {pfHtml = roleField metadata},
    -- Color Scheme
    ValidatedRadioField
      { vrfName = "color_scheme",
        vrfLabel = "Color Scheme",
        vrfOptions = colorSchemeOptions metadata,
        vrfHint = Just "Choose how the site appears to you",
        vrfValidation = emptyValidation
      }
  ]

-- | Email field (read-only)
emailField :: User.Model -> Lucid.Html ()
emailField user =
  Lucid.div_ $ do
    Lucid.label_ [Lucid.for_ "email", class_ $ base ["block", T.fontBold, T.mb2]] "Email"
    Lucid.input_
      [ Lucid.type_ "email",
        Lucid.name_ "email",
        Lucid.id_ "email",
        Lucid.value_ (display user.mEmail),
        class_ $ base [T.fullWidth, T.p3, T.border2, "border-gray-300", T.bgGray100, T.textGray600],
        Lucid.disabled_ "disabled"
      ]
    Lucid.p_ [class_ $ base [T.textSm, "text-gray-500", "mt-1"]] "Email cannot be changed"

-- | Avatar upload field using ImageFilePicker component
avatarField :: UserMetadata.Model -> Lucid.Html ()
avatarField metadata =
  ImageFilePicker.render
    ImageFilePicker.Config
      { fieldName = "avatar",
        label = "Avatar Image",
        existingImageUrl = maybe "" (\url -> [i|/#{url}|]) metadata.mAvatarUrl,
        accept = "image/jpeg,image/jpg,image/png,image/webp,image/gif",
        maxSizeMB = 10,
        isRequired = False
      }

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

-- | Color scheme radio options
colorSchemeOptions :: UserMetadata.Model -> [SelectOption]
colorSchemeOptions metadata =
  [ SelectOption
      { soValue = "Automatic",
        soLabel = "Automatic",
        soSelected = metadata.mColorScheme == UserMetadata.Automatic,
        soDescription = Just "Follows system preference"
      },
    SelectOption
      { soValue = "LightMode",
        soLabel = "Light Mode",
        soSelected = metadata.mColorScheme == UserMetadata.LightMode,
        soDescription = Just "Always use light theme"
      },
    SelectOption
      { soValue = "DarkMode",
        soLabel = "Dark Mode",
        soSelected = metadata.mColorScheme == UserMetadata.DarkMode,
        soDescription = Just "Always use dark theme"
      }
  ]

-- | Submit and cancel buttons
submitButtons :: Lucid.Html ()
submitButtons =
  Lucid.div_ [class_ $ base ["flex", T.gap4, "pt-4", T.border2, "border-t-gray-200"]] $ do
    Lucid.button_
      [ Lucid.type_ "submit",
        class_ $ base [T.buttonPrimary]
      ]
      "SAVE CHANGES"
    Lucid.a_
      [ Lucid.href_ [i|/#{dashboardEpisodesRedirectUrl}|],
        hxGet_ [i|/#{dashboardEpisodesRedirectUrl}|],
        hxPushUrl_ "true",
        class_ $ base ["bg-gray-300", T.textGray800, T.px6, "py-3", T.fontBold, "hover:bg-gray-400"]
      ]
      "CANCEL"
