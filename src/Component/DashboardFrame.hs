{-# LANGUAGE QuasiQuotes #-}

module Component.DashboardFrame
  ( template,
    loadDashboardFrame,
    loadDashboardContentOnly,
    DashboardNav (..),
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (dashboardBlogsGetLink, dashboardEpisodesGetLink, dashboardShowsGetLink, dashboardUsersGetLink, rootGetLink, userLogoutGetLink)
import Component.Banner (bannerContainerId)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Lucid.Extras (onchange_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI rootGetLink

userLogoutGetUrl :: Links.URI
userLogoutGetUrl = Links.linkURI userLogoutGetLink

dashboardEpisodesGetUrl :: Slug -> Links.URI
dashboardEpisodesGetUrl slug = Links.linkURI $ dashboardEpisodesGetLink slug

dashboardBlogsGetUrl :: Slug -> Links.URI
dashboardBlogsGetUrl mSlug = Links.linkURI $ dashboardBlogsGetLink mSlug

dashboardUsersGetUrl :: Links.URI
dashboardUsersGetUrl = Links.linkURI dashboardUsersGetLink

dashboardShowsGetUrl :: Links.URI
dashboardShowsGetUrl = Links.linkURI dashboardShowsGetLink

--------------------------------------------------------------------------------

-- | Which navigation item is currently active
data DashboardNav
  = NavEpisodes
  | NavBlog
  | NavSchedule
  | NavSettings
  | NavUsers
  | NavShows
  deriving (Eq, Show)

-- | Dashboard frame template - full page liquid layout with sidebar
template ::
  UserMetadata.Model ->
  [Shows.Model] ->
  Maybe Shows.Model ->
  DashboardNav ->
  Lucid.Html () ->
  Lucid.Html ()
template userMeta allShows selectedShow activeNav main =
  Lucid.doctypehtml_ $ do
    Lucid.head_ $ do
      Lucid.title_ "Dashboard | KPBJ 95.9FM"
      Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.7.1/css/all.min.css"]
      Lucid.script_ [Lucid.src_ "https://cdn.tailwindcss.com"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "https://unpkg.com/htmx.org@2.0.0"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "https://cdn.jsdelivr.net/npm/flowbite@2.5.2/dist/flowbite.min.js"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "//unpkg.com/alpinejs", Lucid.defer_ "true"] (mempty @Text)
      Lucid.script_ [] ("tailwind.config = { theme: { fontFamily: { sans: ['ui-monospace', 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', 'Courier New', 'monospace'], mono: ['ui-monospace', 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', 'Courier New', 'monospace'] } } }" :: Text)
    Lucid.body_ [Lucid.class_ "font-mono text-gray-800 bg-gray-100"] $ do
      -- Full height flex container
      Lucid.div_ [Lucid.class_ "min-h-screen flex"] $ do
        -- Left sidebar
        sidebar userMeta allShows selectedShow activeNav
        -- Main content area
        Lucid.div_ [Lucid.class_ "flex-1 flex flex-col"] $ do
          -- Top bar
          topBar userMeta selectedShow
          -- Banner container
          Lucid.div_ [Lucid.class_ "px-8"] $
            Lucid.div_ [Lucid.id_ bannerContainerId, Lucid.class_ "w-full"] mempty
          -- Main content (use same ID as main site for HTMX compatibility)
          Lucid.main_ [Lucid.class_ "flex-1 p-8", Lucid.id_ "main-content"] main

-- | Left sidebar with navigation
sidebar ::
  UserMetadata.Model ->
  [Shows.Model] ->
  Maybe Shows.Model ->
  DashboardNav ->
  Lucid.Html ()
sidebar userMeta allShows selectedShow activeNav =
  Lucid.aside_ [Lucid.class_ "w-64 bg-gray-900 text-white flex flex-col h-screen sticky top-0"] $ do
    -- Logo / Brand
    Lucid.div_ [Lucid.class_ "p-4 border-b border-gray-700 shrink-0"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{rootGetUrl}|],
          Lucid.class_ "text-lg font-bold hover:text-gray-300 block"
        ]
        "KPBJ 95.9FM"
      Lucid.span_ [Lucid.class_ "text-xs text-gray-500 block mt-1"] "Dashboard"

    -- Show selector (if multiple shows)
    showSelector allShows selectedShow activeNav

    -- Navigation (scrollable if needed)
    Lucid.nav_ [Lucid.class_ "flex-1 p-4 overflow-y-auto"] $ do
      Lucid.ul_ [Lucid.class_ "space-y-2"] $ do
        navItem "EPISODES" NavEpisodes activeNav selectedShow
        navItem "BLOG" NavBlog activeNav selectedShow

      -- Staff/Admin section - shown only for Staff or higher roles
      when (UserMetadata.isStaffOrHigher userMeta.mUserRole) $ do
        Lucid.div_ [Lucid.class_ "border-t border-gray-700 mt-4 pt-4"] $ do
          Lucid.span_ [Lucid.class_ "text-xs text-gray-500 block px-4 mb-2"] "ADMIN"
          Lucid.ul_ [Lucid.class_ "space-y-2"] $ do
            staffNavItem "USERS" NavUsers activeNav
            staffNavItem "SHOWS" NavShows activeNav

    -- User info at bottom (always visible)
    Lucid.div_ [Lucid.class_ "p-4 border-t border-gray-700 shrink-0 mt-auto"] $ do
      Lucid.div_ [Lucid.class_ "text-sm text-gray-400 mb-2"] $ do
        Lucid.span_ [Lucid.class_ "block font-bold text-white"] $
          Lucid.toHtml userMeta.mDisplayName
        Lucid.span_ [Lucid.class_ "text-xs"] $
          Lucid.toHtml $
            Text.pack $
              show userMeta.mUserRole
      Lucid.a_
        [Lucid.href_ [i|/#{userLogoutGetUrl}|], Lucid.class_ "text-sm text-gray-400 hover:text-white"]
        "Logout"

-- | Show selector dropdown in sidebar
--
-- When changed, navigates to the same tab but with the new show selected.
-- For admin pages (Users/Shows), changing the show navigates to Episodes instead.
showSelector :: [Shows.Model] -> Maybe Shows.Model -> DashboardNav -> Lucid.Html ()
showSelector allShows selectedShow activeNav =
  case allShows of
    [] -> mempty
    [singleShow] ->
      -- Single show - just display it
      Lucid.div_ [Lucid.class_ "p-4 border-b border-gray-700"] $ do
        Lucid.span_ [Lucid.class_ "text-xs text-gray-500 block mb-1"] "SHOW"
        Lucid.span_ [Lucid.class_ "font-bold block truncate"] $
          Lucid.toHtml singleShow.title
    _ -> do
      -- Multiple shows - dropdown with JS redirect
      -- For admin pages, redirect to Episodes when show changes
      -- URL pattern for Episodes: /dashboard/episodes/:slug (slug in path)
      -- URL pattern for Blog: /dashboard/blog?show=:slug (slug as query param)
      let baseUrlPrefix = showSelectorTargetUrl activeNav
          urlPattern :: Text
          urlPattern = case activeNav of
            NavEpisodes -> [i|'/#{baseUrlPrefix}/' + this.value|]
            _ -> [i|'/#{baseUrlPrefix}?show=' + this.value|]
      Lucid.div_ [Lucid.class_ "p-4 border-b border-gray-700"] $ do
        Lucid.label_ [Lucid.for_ "show-selector", Lucid.class_ "text-xs text-gray-500 block mb-1"] "SHOW"
        Lucid.select_
          [ Lucid.id_ "show-selector",
            Lucid.name_ "show",
            Lucid.class_ "w-full p-2 bg-gray-800 border border-gray-600 text-white text-sm font-bold",
            onchange_ [i|window.location.href = #{urlPattern}|]
          ]
          $ mapM_ (renderShowOption selectedShow) allShows

-- | Get the base URL path to navigate to when show selector changes
-- For show-specific pages (Episodes, Blog), stay on the same page
-- For admin pages (Users, Shows), navigate to Episodes
-- Returns just the base path - the slug will be appended in JavaScript
showSelectorTargetUrl :: DashboardNav -> Text
showSelectorTargetUrl = \case
  NavEpisodes -> "dashboard/episodes"
  NavBlog -> "dashboard/blog"
  NavSchedule -> "dashboard/episodes"
  NavSettings -> "dashboard/episodes"
  -- Admin pages: changing show goes to Episodes
  NavUsers -> "dashboard/episodes"
  NavShows -> "dashboard/episodes"

-- | Render a single show option
renderShowOption :: Maybe Shows.Model -> Shows.Model -> Lucid.Html ()
renderShowOption selectedShow showModel = do
  let isSelected = maybe False (\s -> Shows.slug s == Shows.slug showModel) selectedShow
      showSlug = Shows.slug showModel
      showTitle = Shows.title showModel
  if isSelected
    then Lucid.option_ [Lucid.value_ (display showSlug), Lucid.selected_ "selected"] (Lucid.toHtml showTitle)
    else Lucid.option_ [Lucid.value_ (display showSlug)] (Lucid.toHtml showTitle)

-- | Navigation item - simple link with server-rendered active state
-- If no URL can be generated (e.g., Episodes without a show), renders as disabled
navItem :: Text -> DashboardNav -> DashboardNav -> Maybe Shows.Model -> Lucid.Html ()
navItem label nav activeNav mShow = do
  let isActive = nav == activeNav
      baseClasses = "block px-4 py-2 text-sm font-bold transition-colors"
      activeClasses = "bg-gray-800 text-white"
      inactiveClasses = "text-gray-400 hover:text-white hover:bg-gray-800"
      disabledClasses = "text-gray-600 cursor-not-allowed"
      mUrl = navUrl nav mShow
  case mUrl of
    Just url ->
      let classes = baseClasses <> " " <> if isActive then activeClasses else inactiveClasses
       in Lucid.li_ $
            Lucid.a_ [Lucid.href_ [i|/#{url}|], Lucid.class_ classes] (Lucid.toHtml label)
    Nothing ->
      let classes = baseClasses <> " " <> disabledClasses
       in Lucid.li_ $
            Lucid.span_ [Lucid.class_ classes] (Lucid.toHtml label)

-- | Staff navigation item - doesn't require show context
-- Uses full page navigation (no HTMX) since admin pages have different sidebar state
staffNavItem :: Text -> DashboardNav -> DashboardNav -> Lucid.Html ()
staffNavItem label nav activeNav = do
  let isActive = nav == activeNav
      baseClasses = "block px-4 py-2 text-sm font-bold transition-colors"
      activeClasses = "bg-gray-800 text-white"
      inactiveClasses = "text-gray-400 hover:text-white hover:bg-gray-800"
  case staffNavUrl nav of
    Just url ->
      let classes = baseClasses <> " " <> if isActive then activeClasses else inactiveClasses
       in Lucid.li_ $
            -- No hxGet_ here - force full page load to get correct sidebar state
            Lucid.a_ [Lucid.href_ [i|/#{url}|], Lucid.class_ classes] (Lucid.toHtml label)
    Nothing -> mempty

-- | Get URL for staff navigation item (no show context)
staffNavUrl :: DashboardNav -> Maybe Links.URI
staffNavUrl = \case
  NavUsers -> Just dashboardUsersGetUrl
  NavShows -> Just dashboardShowsGetUrl
  other -> navUrl other Nothing

-- | Get URL for navigation item
-- Episodes requires a show slug in the path, Blog uses query param
-- Returns Nothing if a required show is not provided
navUrl :: DashboardNav -> Maybe Shows.Model -> Maybe Links.URI
navUrl nav mShow =
  let mSlug = Shows.slug <$> mShow
   in case nav of
        NavEpisodes -> dashboardEpisodesGetUrl <$> mSlug
        NavBlog -> dashboardBlogsGetUrl <$> mSlug
        NavSchedule -> dashboardEpisodesGetUrl <$> mSlug -- TODO: Add schedule route
        NavSettings -> dashboardEpisodesGetUrl <$> mSlug -- TODO: Add settings route
        NavUsers -> Just dashboardUsersGetUrl
        NavShows -> Just dashboardShowsGetUrl

-- | Top bar with breadcrumb and actions
topBar :: UserMetadata.Model -> Maybe Shows.Model -> Lucid.Html ()
topBar _userMeta selectedShow =
  Lucid.header_ [Lucid.class_ "bg-white border-b border-gray-200 px-8 py-4"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      -- Breadcrumb
      Lucid.div_ [Lucid.class_ "flex items-center gap-2 text-sm"] $ do
        Lucid.span_ [Lucid.class_ "text-gray-400"] "Dashboard"
        case selectedShow of
          Just showModel -> do
            Lucid.span_ [Lucid.class_ "text-gray-400"] "/"
            Lucid.span_ [Lucid.class_ "font-bold"] $ Lucid.toHtml showModel.title
          Nothing -> mempty
      -- Back to site link
      Lucid.a_
        [ Lucid.href_ [i|/#{rootGetUrl}|],
          Lucid.class_ "text-sm text-gray-500 hover:text-gray-800"
        ]
        "Back to Site"

--------------------------------------------------------------------------------

-- | Load dashboard frame (full page)
loadDashboardFrame ::
  (Log.MonadLog m, MonadThrow m) =>
  UserMetadata.Model ->
  [Shows.Model] ->
  Maybe Shows.Model ->
  DashboardNav ->
  Lucid.Html () ->
  m (Lucid.Html ())
loadDashboardFrame user allShows selectedShow nav content =
  pure $ template user allShows selectedShow nav content

-- | Load content only for HTMX requests
loadDashboardContentOnly :: (Log.MonadLog m, MonadThrow m) => Lucid.Html () -> m (Lucid.Html ())
loadDashboardContentOnly = pure
