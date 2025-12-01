{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Users.Edit.Get.Templates.Page where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (dashboardUserDetailGetLink, dashboardUserEditPostLink)
import Data.String.Interpolate (i)
import Data.Text.Display (display)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Links

template :: User.Model -> UserMetadata.Model -> Lucid.Html ()
template user metadata = do
  -- Back button
  Lucid.div_ [Lucid.class_ "mb-8"] $ do
    Lucid.a_
      [ Lucid.href_ [i|/#{backUrl}|],
        hxGet_ [i|/#{backUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "text-blue-600 hover:underline font-bold mb-4 inline-block"
      ]
      "← Back to User Detail"

  -- Page header
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 w-full"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold"] "EDIT USER"
    Lucid.p_ [Lucid.class_ "text-gray-600 mt-2"] $
      Lucid.toHtml $
        "Editing: " <> display user.mEmail

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
      -- Display Name
      Lucid.div_ $ do
        Lucid.label_ [Lucid.for_ "display_name", Lucid.class_ "block font-bold mb-2"] "Display Name *"
        Lucid.input_
          [ Lucid.type_ "text",
            Lucid.name_ "display_name",
            Lucid.id_ "display_name",
            Lucid.value_ (display metadata.mDisplayName),
            Lucid.class_ "w-full p-3 border-2 border-gray-800",
            Lucid.required_ ""
          ]

      -- Full Name
      Lucid.div_ $ do
        Lucid.label_ [Lucid.for_ "full_name", Lucid.class_ "block font-bold mb-2"] "Full Name *"
        Lucid.input_
          [ Lucid.type_ "text",
            Lucid.name_ "full_name",
            Lucid.id_ "full_name",
            Lucid.value_ (display metadata.mFullName),
            Lucid.class_ "w-full p-3 border-2 border-gray-800",
            Lucid.required_ ""
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
                  Lucid.class_ "w-24 h-24 object-cover border-2 border-gray-800"
                ]
          Nothing -> do
            Lucid.p_ [Lucid.class_ "text-sm text-gray-600 mb-2"] "No avatar uploaded"

        Lucid.input_
          [ Lucid.type_ "file",
            Lucid.name_ "avatar",
            Lucid.id_ "avatar",
            Lucid.class_ "w-full p-3 border-2 border-gray-800",
            Lucid.accept_ "image/jpeg,image/jpg,image/png,image/webp,image/gif"
          ]
        Lucid.p_ [Lucid.class_ "text-sm text-gray-600 mt-1"] "Optional: Upload a new avatar image (JPEG, PNG, WebP, or GIF, max 10MB)"

      -- Role Selector
      Lucid.div_ $ do
        Lucid.label_ [Lucid.for_ "role", Lucid.class_ "block font-bold mb-2"] "Role *"
        Lucid.select_
          [ Lucid.name_ "role",
            Lucid.id_ "role",
            Lucid.class_ "w-full p-3 border-2 border-gray-800"
          ]
          $ do
            renderRoleOption UserMetadata.User metadata.mUserRole
            renderRoleOption UserMetadata.Host metadata.mUserRole
            renderRoleOption UserMetadata.Staff metadata.mUserRole
            renderRoleOption UserMetadata.Admin metadata.mUserRole

      -- Info box
      Lucid.div_ [Lucid.class_ "bg-blue-50 border-2 border-blue-200 p-4"] $ do
        Lucid.p_ [Lucid.class_ "text-sm text-blue-800"] $ do
          Lucid.strong_ "Note: "
          "Changing user roles affects their permissions. "
          "Admin has full access, Staff can manage content, Host can manage shows, User has basic access."

      -- Submit buttons
      Lucid.div_ [Lucid.class_ "flex gap-4 pt-4 border-t-2 border-gray-200"] $ do
        Lucid.button_
          [ Lucid.type_ "submit",
            Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
          ]
          "SAVE CHANGES"

        Lucid.a_
          [ Lucid.href_ [i|/#{backUrl}|],
            hxGet_ [i|/#{backUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "bg-gray-300 text-gray-800 px-6 py-3 font-bold hover:bg-gray-400"
          ]
          "CANCEL"
  where
    backUrl = Links.linkURI $ dashboardUserDetailGetLink user.mId
    postUrl = Links.linkURI $ dashboardUserEditPostLink user.mId

renderRoleOption :: UserMetadata.UserRole -> UserMetadata.UserRole -> Lucid.Html ()
renderRoleOption roleOption currentRole = do
  let roleText = case roleOption of
        UserMetadata.User -> "User"
        UserMetadata.Host -> "Host"
        UserMetadata.Staff -> "Staff"
        UserMetadata.Admin -> "Admin"

  Lucid.option_
    [ Lucid.value_ roleText,
      if roleOption == currentRole then Lucid.selected_ "selected" else mempty
    ]
    $ Lucid.toHtml roleText
