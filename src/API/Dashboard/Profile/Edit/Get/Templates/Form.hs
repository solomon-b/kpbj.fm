{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Profile.Edit.Get.Templates.Form where

--------------------------------------------------------------------------------

import API.Links (dashboardLinks)
import API.Types (DashboardRoutes (..))
import Data.String.Interpolate (i)
import Data.Text.Display (display)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
dashboardEpisodesRedirectUrl :: Links.URI
dashboardEpisodesRedirectUrl = Links.linkURI dashboardLinks.episodesRedirect

--------------------------------------------------------------------------------

template :: User.Model -> UserMetadata.Model -> Lucid.Html ()
template user metadata = do
  -- Page header
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 w-full"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold"] "EDIT PROFILE"
    Lucid.p_ [Lucid.class_ "text-gray-600 mt-2"]
      "Update your profile information"

  -- Edit form
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 w-full"]
    $ Lucid.form_
      [ Lucid.action_ [i|/#{postUrl}|],
        Lucid.method_ "post",
        Lucid.enctype_ "multipart/form-data",
        hxPost_ [i|/#{postUrl}|],
        hxTarget_ "#main-content",
        hxSwap_ "innerHTML",
        Lucid.class_ "space-y-6"
      ]
    $ do
      -- Email (read-only)
      Lucid.div_ $ do
        Lucid.label_ [Lucid.for_ "email", Lucid.class_ "block font-bold mb-2"] "Email"
        Lucid.input_
          [ Lucid.type_ "email",
            Lucid.name_ "email",
            Lucid.id_ "email",
            Lucid.value_ (display user.mEmail),
            Lucid.class_ "w-full p-3 border-2 border-gray-300 bg-gray-100 text-gray-600",
            Lucid.disabled_ "disabled"
          ]
        Lucid.p_ [Lucid.class_ "text-sm text-gray-500 mt-1"] "Email cannot be changed"

      -- Display Name
      Lucid.div_ $ do
        Lucid.label_ [Lucid.for_ "display_name", Lucid.class_ "block font-bold mb-2"] "Display Name *"
        Lucid.input_
          [ Lucid.type_ "text",
            Lucid.name_ "display_name",
            Lucid.id_ "display_name",
            Lucid.value_ (display metadata.mDisplayName),
            Lucid.class_ "w-full p-3 border-2 border-gray-800",
            Lucid.required_ "",
            Lucid.placeholder_ "Your display name"
          ]
        Lucid.p_ [Lucid.class_ "text-sm text-gray-500 mt-1"] "This name will be shown publicly on the site"

      -- Full Name
      Lucid.div_ $ do
        Lucid.label_ [Lucid.for_ "full_name", Lucid.class_ "block font-bold mb-2"] "Full Name *"
        Lucid.input_
          [ Lucid.type_ "text",
            Lucid.name_ "full_name",
            Lucid.id_ "full_name",
            Lucid.value_ (display metadata.mFullName),
            Lucid.class_ "w-full p-3 border-2 border-gray-800",
            Lucid.required_ "",
            Lucid.placeholder_ "Your full name"
          ]

      -- Avatar Upload
      Lucid.div_ $ do
        Lucid.label_ [Lucid.for_ "avatar", Lucid.class_ "block font-bold mb-2"] "Avatar Image"

        -- Show current avatar if it exists
        case metadata.mAvatarUrl of
          Just avatarUrl -> do
            Lucid.div_ [Lucid.class_ "mb-4"] $ do
              Lucid.p_ [Lucid.class_ "text-sm text-gray-600 mb-2"] "Current avatar:"
              Lucid.img_
                [ Lucid.src_ [i|/#{avatarUrl}|],
                  Lucid.alt_ "Current avatar",
                  Lucid.class_ "w-24 h-24 object-cover border-2 border-gray-800 rounded-full"
                ]
          Nothing -> do
            Lucid.div_ [Lucid.class_ "mb-4"] $ do
              Lucid.p_ [Lucid.class_ "text-sm text-gray-600 mb-2"] "No avatar uploaded"
              Lucid.div_ [Lucid.class_ "w-24 h-24 bg-gray-200 border-2 border-gray-800 rounded-full flex items-center justify-center"] $
                Lucid.span_ [Lucid.class_ "text-gray-500 text-3xl font-bold"] $
                  Lucid.toHtml $
                    take 1 $
                      show metadata.mDisplayName

        Lucid.input_
          [ Lucid.type_ "file",
            Lucid.name_ "avatar",
            Lucid.id_ "avatar",
            Lucid.class_ "w-full p-3 border-2 border-gray-800",
            Lucid.accept_ "image/jpeg,image/jpg,image/png,image/webp,image/gif"
          ]
        Lucid.p_ [Lucid.class_ "text-sm text-gray-500 mt-1"] "Upload a new avatar image (JPEG, PNG, WebP, or GIF, max 10MB)"

      -- Role display (read-only)
      Lucid.div_ $ do
        Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Role"
        Lucid.div_ [Lucid.class_ "p-3 border-2 border-gray-300 bg-gray-100"] $ do
          Lucid.span_ [Lucid.class_ "inline-flex items-center px-3 py-1 text-sm font-bold bg-gray-800 text-white"] $
            Lucid.toHtml $
              show metadata.mUserRole
        Lucid.p_ [Lucid.class_ "text-sm text-gray-500 mt-1"] "Role can only be changed by administrators"

      -- Submit buttons
      Lucid.div_ [Lucid.class_ "flex gap-4 pt-4 border-t-2 border-gray-200"] $ do
        Lucid.button_
          [ Lucid.type_ "submit",
            Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
          ]
          "SAVE CHANGES"

        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardEpisodesRedirectUrl}|],
            hxGet_ [i|/#{dashboardEpisodesRedirectUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "bg-gray-300 text-gray-800 px-6 py-3 font-bold hover:bg-gray-400"
          ]
          "CANCEL"
  where
    postUrl = Links.linkURI dashboardLinks.profileEditPost
