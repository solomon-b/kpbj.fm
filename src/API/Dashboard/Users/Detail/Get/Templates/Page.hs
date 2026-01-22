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
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras
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
  Lucid.section_ [Lucid.class_ "bg-white dark:bg-gray-800 border-2 border-gray-800 dark:border-gray-600 p-8 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex justify-between items-start"] $ do
      -- Avatar and name
      Lucid.div_ [Lucid.class_ "flex items-center gap-6"] $ do
        renderHeaderAvatar backend metadata.mAvatarUrl
        Lucid.div_ $ do
          Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-2"] $
            Lucid.toHtml (display metadata.mDisplayName)
          Lucid.p_ [Lucid.class_ "text-gray-600 dark:text-gray-400 text-lg"] $
            Lucid.toHtml (display user.mEmail)

    -- User metadata grid
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 gap-6 mt-6 pt-6 border-t-2 border-gray-200 dark:border-gray-600"] $ do
      renderField "Full Name" (Text.unpack $ display metadata.mFullName)
      renderRoleField user.mId metadata.mUserRole

  -- Shows section
  unless (null userShows) $ do
    Lucid.section_ [Lucid.class_ "bg-white dark:bg-gray-800 border-2 border-gray-800 dark:border-gray-600 p-8 mb-8 w-full"] $ do
      Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4"] "SHOWS"
      Lucid.div_ [Lucid.class_ "space-y-4"] $
        mapM_ renderShowCard userShows

  -- Episodes section
  unless (null userEpisodes) $ do
    Lucid.section_ [Lucid.class_ "bg-white dark:bg-gray-800 border-2 border-gray-800 dark:border-gray-600 p-8 mb-8 w-full"] $ do
      Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4"] "RECENT EPISODES"
      Lucid.div_ [Lucid.class_ "space-y-4"] $
        mapM_ renderEpisodeCard userEpisodes

renderField :: String -> String -> Lucid.Html ()
renderField label value = do
  Lucid.div_ $ do
    Lucid.dt_ [Lucid.class_ "font-bold text-gray-700 dark:text-gray-300 mb-1"] $ Lucid.toHtml label
    Lucid.dd_ [Lucid.class_ "text-gray-900 dark:text-gray-100"] $ Lucid.toHtml value

renderHeaderAvatar :: StorageBackend -> Maybe Text.Text -> Lucid.Html ()
renderHeaderAvatar backend mAvatarPath =
  case mAvatarPath of
    Nothing ->
      -- Placeholder avatar
      Lucid.div_
        [ Lucid.class_ "w-20 h-20 rounded-full bg-gray-200 dark:bg-gray-700 border-2 border-gray-800 dark:border-gray-600 flex items-center justify-center"
        ]
        $ Lucid.span_ [Lucid.class_ "text-2xl text-gray-500 dark:text-gray-400"] "?"
    Just path ->
      Lucid.img_
        [ Lucid.src_ (buildMediaUrl backend path),
          Lucid.alt_ "User avatar",
          Lucid.class_ "w-20 h-20 rounded-full object-cover border-2 border-gray-800 dark:border-gray-600"
        ]

renderRoleField :: User.Id -> UserMetadata.UserRole -> Lucid.Html ()
renderRoleField userId currentRole = do
  Lucid.div_ $ do
    Lucid.dt_ [Lucid.class_ "font-bold text-gray-700 dark:text-gray-300 mb-1"] "Role"
    Lucid.dd_ [Lucid.id_ "role-dropdown-container", Lucid.class_ "flex items-center gap-3"] $
      renderRoleDropdown userId currentRole

renderRoleDropdown :: User.Id -> UserMetadata.UserRole -> Lucid.Html ()
renderRoleDropdown userId currentRole =
  Lucid.select_
    [ Lucid.name_ "role",
      Lucid.class_ "p-2 border-2 border-gray-800 bg-white dark:bg-gray-800 font-bold",
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
  Lucid.div_ [Lucid.class_ "border-2 border-gray-200 dark:border-gray-600 p-4 hover:bg-gray-50 dark:hover:bg-gray-700"] $ do
    Lucid.h3_ [Lucid.class_ "font-bold text-lg"] $ Lucid.toHtml showModel.title

renderEpisodeCard :: Episodes.Model -> Lucid.Html ()
renderEpisodeCard episode = do
  Lucid.div_ [Lucid.class_ "border-2 border-gray-200 dark:border-gray-600 p-4 hover:bg-gray-50 dark:hover:bg-gray-700"] $ do
    Lucid.h3_ [Lucid.class_ "font-bold text-lg"] $ Lucid.toHtml (("Episode " <> show episode.episodeNumber) :: String)
    Lucid.p_ [Lucid.class_ "text-gray-600 dark:text-gray-400 text-sm mt-1"] $
      Lucid.toHtml $
        "Status: " <> renderStatus episode.status
    when (isJust episode.publishedAt) $
      Lucid.p_ [Lucid.class_ "text-gray-600 dark:text-gray-400 text-sm"] $
        Lucid.toHtml $
          "Published: " <> formatDate (fromMaybe episode.createdAt episode.publishedAt)
  where
    when cond action = if cond then action else mempty

renderStatus :: Episodes.Status -> String
renderStatus Episodes.Draft = "Draft"
renderStatus Episodes.Published = "Published"
renderStatus Episodes.Deleted = "Deleted"

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
