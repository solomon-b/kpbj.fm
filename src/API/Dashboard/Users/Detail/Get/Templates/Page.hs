{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Users.Detail.Get.Templates.Page where

--------------------------------------------------------------------------------

import API.Links (dashboardUsersLinks)
import API.Types
import Control.Monad (unless)
import Data.Maybe (fromMaybe, isJust)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

template ::
  StorageBackend ->
  User.Model ->
  UserMetadata.Model ->
  [Shows.Model] ->
  [Episodes.Model] ->
  Lucid.Html ()
template backend user metadata userShows userEpisodes = do
  -- User profile section
  Lucid.section_ [class_ $ base [Tokens.bgMain, Tokens.border2, Tokens.borderDefault, Tokens.p8, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "justify-between", "items-start"]] $ do
      -- Avatar and name
      Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap6]] $ do
        renderHeaderAvatar backend metadata.mAvatarUrl
        Lucid.div_ $ do
          Lucid.h1_ [class_ $ base [Tokens.text3xl, Tokens.fontBold, Tokens.mb2]] $
            Lucid.toHtml (display metadata.mDisplayName)
          Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.textLg]] $
            Lucid.toHtml (display user.mEmail)

    -- User metadata grid
    Lucid.div_ [class_ $ base ["grid", "grid-cols-1", "md:grid-cols-2", Tokens.gap6, "mt-6", "pt-6", "border-t-2", Tokens.borderMuted]] $ do
      renderField "Full Name" (Text.unpack $ display metadata.mFullName)
      renderRoleField user.mId metadata.mUserRole

  -- Shows section
  unless (null userShows) $ do
    Lucid.section_ [class_ $ base [Tokens.bgMain, Tokens.border2, Tokens.borderDefault, Tokens.p8, Tokens.mb8, Tokens.fullWidth]] $ do
      Lucid.h2_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb4]] "SHOWS"
      Lucid.div_ [class_ $ base ["space-y-4"]] $
        mapM_ renderShowCard userShows

  -- Episodes section
  unless (null userEpisodes) $ do
    Lucid.section_ [class_ $ base [Tokens.bgMain, Tokens.border2, Tokens.borderDefault, Tokens.p8, Tokens.mb8, Tokens.fullWidth]] $ do
      Lucid.h2_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb4]] "RECENT EPISODES"
      Lucid.div_ [class_ $ base ["space-y-4"]] $
        mapM_ renderEpisodeCard userEpisodes

renderField :: String -> String -> Lucid.Html ()
renderField label value = do
  Lucid.div_ $ do
    Lucid.dt_ [class_ $ base [Tokens.fontBold, Tokens.fgPrimary, "mb-1"]] $ Lucid.toHtml label
    Lucid.dd_ [class_ $ base [Tokens.fgPrimary]] $ Lucid.toHtml value

renderHeaderAvatar :: StorageBackend -> Maybe Text.Text -> Lucid.Html ()
renderHeaderAvatar backend mAvatarPath =
  case mAvatarPath of
    Nothing ->
      -- Placeholder avatar
      Lucid.div_
        [ class_ $ base ["w-20", "h-20", "rounded-full", Tokens.bgAlt, Tokens.border2, Tokens.borderDefault, "flex", "items-center", "justify-center"]
        ]
        $ Lucid.span_ [class_ $ base [Tokens.text2xl, Tokens.fgMuted]] "?"
    Just path ->
      Lucid.img_
        [ Lucid.src_ (buildMediaUrl backend path),
          Lucid.alt_ "User avatar",
          class_ $ base ["w-20", "h-20", "rounded-full", "object-cover", Tokens.border2, Tokens.borderDefault]
        ]

renderRoleField :: User.Id -> UserMetadata.UserRole -> Lucid.Html ()
renderRoleField userId currentRole = do
  Lucid.div_ $ do
    Lucid.dt_ [class_ $ base [Tokens.fontBold, Tokens.fgPrimary, "mb-1"]] "Role"
    Lucid.dd_ [Lucid.id_ "role-dropdown-container", class_ $ base ["flex", "items-center", "gap-3"]] $
      renderRoleDropdown userId currentRole

renderRoleDropdown :: User.Id -> UserMetadata.UserRole -> Lucid.Html ()
renderRoleDropdown userId currentRole =
  Lucid.select_
    [ Lucid.name_ "role",
      class_ $ base [Tokens.p2, Tokens.border2, Tokens.borderDefault, Tokens.bgMain, Tokens.fgPrimary, Tokens.fontBold],
      hxPatch_ [i|/#{rolePatchUrl}|],
      hxTarget_ "#role-dropdown-container",
      hxSwap_ "innerHTML"
    ]
    $ do
      roleOption UserMetadata.User currentRole
      roleOption UserMetadata.Host currentRole
      roleOption UserMetadata.Staff currentRole
      roleOption UserMetadata.Admin currentRole
  where
    rolePatchUrl = Links.linkURI $ dashboardUsersLinks.rolePatch userId

    roleOption :: UserMetadata.UserRole -> UserMetadata.UserRole -> Lucid.Html ()
    roleOption role selected =
      Lucid.option_
        ( [Lucid.value_ (display role)]
            <> [Lucid.selected_ "selected" | role == selected]
        )
        $ Lucid.toHtml (display role)

renderShowCard :: Shows.Model -> Lucid.Html ()
renderShowCard showModel = do
  Lucid.div_ [class_ $ base [Tokens.border2, Tokens.borderMuted, Tokens.p4, Tokens.hoverBg]] $ do
    Lucid.h3_ [class_ $ base [Tokens.fontBold, Tokens.textLg]] $ Lucid.toHtml showModel.title

renderEpisodeCard :: Episodes.Model -> Lucid.Html ()
renderEpisodeCard episode = do
  Lucid.div_ [class_ $ base [Tokens.border2, Tokens.borderMuted, Tokens.p4, Tokens.hoverBg]] $ do
    Lucid.h3_ [class_ $ base [Tokens.fontBold, Tokens.textLg]] $ Lucid.toHtml (("Episode " <> show episode.episodeNumber) :: String)
    -- Show archived status if applicable
    when (isJust episode.deletedAt) $
      Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.textSm, "mt-1"]] $
        Lucid.toHtml ("Status: Archived" :: String)
    when (isJust episode.publishedAt) $
      Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.textSm]] $
        Lucid.toHtml $
          "Published: " <> formatDate (fromMaybe episode.createdAt episode.publishedAt)
  where
    when cond action = if cond then action else mempty

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%Y-%m-%d %l:%M %p"
