{-# LANGUAGE QuasiQuotes #-}

module Component.DashboardFrame
  ( template,
    loadDashboardFrame,
    loadDashboardContentOnly,
    DashboardNav (..),
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, dashboardBlogsLinks, dashboardEpisodesLinks, dashboardEventsLinks, dashboardLinks, dashboardShowsLinks, dashboardStationBlogLinks, dashboardUsersLinks, userLinks)
import API.Types
import Component.Banner (bannerContainerId)
import Component.Frame (bannerFromUrlScript, googleAnalyticsScript)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Design (base, class_, class_')
import Design.Tokens qualified as Tokens
import Domain.Types.GoogleAnalyticsId (GoogleAnalyticsId)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, onchange_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

userLogoutGetUrl :: Links.URI
userLogoutGetUrl = Links.linkURI userLinks.logoutGet

dashboardProfileEditUrl :: Links.URI
dashboardProfileEditUrl = Links.linkURI dashboardLinks.profileEditGet

dashboardEpisodesGetUrl :: Slug -> Links.URI
dashboardEpisodesGetUrl slug = Links.linkURI $ dashboardEpisodesLinks.list slug Nothing

dashboardBlogsGetUrl :: Slug -> Links.URI
dashboardBlogsGetUrl mSlug = Links.linkURI $ dashboardBlogsLinks.list mSlug Nothing

dashboardUsersGetUrl :: Links.URI
dashboardUsersGetUrl = Links.linkURI $ dashboardUsersLinks.list Nothing Nothing Nothing Nothing

dashboardShowsGetUrl :: Links.URI
dashboardShowsGetUrl = Links.linkURI $ dashboardShowsLinks.list Nothing Nothing Nothing

dashboardStationBlogGetUrl :: Links.URI
dashboardStationBlogGetUrl = Links.linkURI $ dashboardStationBlogLinks.list Nothing

dashboardEventsGetUrl :: Links.URI
dashboardEventsGetUrl = Links.linkURI $ dashboardEventsLinks.list Nothing

dashboardShowSettingsUrl :: Slug -> Links.URI
dashboardShowSettingsUrl slug = Links.linkURI $ dashboardShowsLinks.editGet slug

--------------------------------------------------------------------------------

-- | Which navigation item is currently active
data DashboardNav
  = NavEpisodes
  | NavBlog
  | NavSchedule
  | NavSettings
  | NavUsers
  | NavShows
  | NavStationBlog
  | NavEvents
  deriving (Eq, Show)

-- | Get display title for navigation item
navTitle :: DashboardNav -> Text
navTitle = \case
  NavEpisodes -> "Episodes"
  NavBlog -> "Blog"
  NavSchedule -> "Schedule"
  NavSettings -> "Settings"
  NavUsers -> "Users"
  NavShows -> "Shows"
  NavStationBlog -> "Station Blog"
  NavEvents -> "Events"

-- | Check if navigation requires show context
isShowScoped :: DashboardNav -> Bool
isShowScoped = \case
  NavEpisodes -> True
  NavBlog -> True
  NavSchedule -> True
  NavSettings -> True
  NavUsers -> False
  NavShows -> False
  NavStationBlog -> False
  NavEvents -> False

-- | Dashboard frame template - full page liquid layout with sidebar
template ::
  Maybe GoogleAnalyticsId ->
  UserMetadata.Model ->
  [Shows.Model] ->
  Maybe Shows.Model ->
  DashboardNav ->
  Maybe (Lucid.Html ()) ->
  Maybe (Lucid.Html ()) ->
  Lucid.Html () ->
  Lucid.Html ()
template mGoogleAnalyticsId userMeta allShows selectedShow activeNav statsContent actionButton main =
  Lucid.doctypehtml_ $ do
    Lucid.head_ $ do
      googleAnalyticsScript mGoogleAnalyticsId
      Lucid.title_ "Dashboard | KPBJ 95.9FM"
      Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.7.1/css/all.min.css"]
      Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ "https://cdnjs.cloudflare.com/ajax/libs/cropperjs/1.6.2/cropper.min.css"]
      Lucid.script_ [Lucid.src_ "https://cdn.tailwindcss.com"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "https://cdnjs.cloudflare.com/ajax/libs/cropperjs/1.6.2/cropper.min.js"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "https://unpkg.com/htmx.org@2.0.0"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "https://cdn.jsdelivr.net/npm/flowbite@2.5.2/dist/flowbite.min.js"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "//unpkg.com/alpinejs", Lucid.defer_ "true"] (mempty @Text)
      Lucid.script_ [] ("tailwind.config = { theme: { fontFamily: { sans: ['ui-monospace', 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', 'Courier New', 'monospace'], mono: ['ui-monospace', 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', 'Courier New', 'monospace'] } } }" :: Text)
    Lucid.body_ [class_ $ base ["font-mono", Tokens.textGray800, Tokens.bgGray100]] $ do
      -- Full height flex container
      Lucid.div_ [class_ $ base ["min-h-screen", "flex"]] $ do
        -- Left sidebar
        sidebar userMeta activeNav selectedShow
        -- Main content area
        Lucid.div_ [class_ $ base ["flex-1", "flex", "flex-col"]] $ do
          -- Top bar with page title, show selector, stats, and action button
          topBar activeNav allShows selectedShow statsContent actionButton
          -- Banner container
          Lucid.div_ [Lucid.class_ Tokens.px8] $
            Lucid.div_ [Lucid.id_ bannerContainerId, Lucid.class_ Tokens.fullWidth] mempty
          -- Main content (use same ID as main site for HTMX compatibility)
          Lucid.main_ [class_ $ base ["flex-1", Tokens.p8], Lucid.id_ "main-content"] main
      -- Script to display banner from URL params (runs after DOM is ready)
      Lucid.script_ [] bannerFromUrlScript

-- | Left sidebar with navigation
sidebar ::
  UserMetadata.Model ->
  DashboardNav ->
  Maybe Shows.Model ->
  Lucid.Html ()
sidebar userMeta activeNav selectedShow =
  Lucid.aside_ [class_ $ base ["w-64", Tokens.bgGray900, Tokens.textWhite, "flex", "flex-col", "h-screen", "sticky", "top-0"]] $ do
    -- Logo / Brand
    Lucid.div_ [class_ $ base [Tokens.p4, "border-b", "border-gray-700", "shrink-0"]] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{rootGetUrl}|],
          class_ $ base [Tokens.textLg, Tokens.fontBold, "hover:text-gray-300", "block"]
        ]
        "KPBJ 95.9FM"
      Lucid.span_ [class_ $ base [Tokens.textXs, "text-gray-500", "block", "mt-1"]] "Dashboard"

    -- Navigation (scrollable if needed)
    Lucid.nav_ [class_ $ base ["flex-1", Tokens.p4, "overflow-y-auto"]] $ do
      Lucid.ul_ [Lucid.class_ "space-y-2"] $ do
        Lucid.span_ [class_ $ base [Tokens.textXs, "text-gray-500", "block", Tokens.px4, Tokens.mb2]] "HOST"
        Lucid.ul_ [Lucid.class_ "space-y-2"] $ do
          navItem "EPISODES" NavEpisodes activeNav selectedShow
          -- navItem "BLOG" NavBlog activeNav selectedShow
          navItem "SHOW SETTINGS" NavSettings activeNav selectedShow

      -- Staff/Admin section - shown only for Staff or higher roles
      when (UserMetadata.isStaffOrHigher userMeta.mUserRole) $ do
        Lucid.div_ [class_ $ base ["border-t", "border-gray-700", "mt-4", "pt-4"]] $ do
          Lucid.span_ [class_ $ base [Tokens.textXs, "text-gray-500", "block", Tokens.px4, Tokens.mb2]] "ADMIN"
          Lucid.ul_ [Lucid.class_ "space-y-2"] $ do
            staffNavItem "USERS" NavUsers activeNav
            staffNavItem "SHOWS" NavShows activeNav
            staffNavItem "STATION BLOG" NavStationBlog activeNav
            staffNavItem "EVENTS" NavEvents activeNav

    -- User info at bottom (always visible)
    Lucid.div_ [class_ $ base [Tokens.p4, "border-t", "border-gray-700", "shrink-0", "mt-auto"]] $ do
      Lucid.div_ [class_ $ base [Tokens.textSm, "text-gray-400", Tokens.mb2]] $ do
        Lucid.span_ [class_ $ base ["block", Tokens.fontBold, Tokens.textWhite]] $
          Lucid.toHtml userMeta.mDisplayName
        Lucid.span_ [Lucid.class_ Tokens.textXs] $
          Lucid.toHtml $
            Text.pack $
              show userMeta.mUserRole
      -- Edit Profile and Logout buttons
      Lucid.div_ [class_ $ base ["flex", "flex-col", Tokens.gap4, "mt-2"]] $ do
        Lucid.a_
          [Lucid.href_ [i|/#{dashboardProfileEditUrl}|], class_ $ base [Tokens.textSm, "text-gray-400", "hover:text-white"]]
          "Settings"
        Lucid.a_
          [Lucid.href_ [i|/#{userLogoutGetUrl}|], class_ $ base [Tokens.textSm, "text-gray-400", "hover:text-white"], hxGet_ [i|/#{userLogoutGetUrl}|]]
          "Logout"

-- | Show selector dropdown for top bar
--
-- When changed, navigates to the same page but with the new show selected.
showSelector :: DashboardNav -> [Shows.Model] -> Maybe Shows.Model -> Lucid.Html ()
showSelector activeNav allShows selectedShow =
  case allShows of
    [] -> mempty
    [singleShow] ->
      -- Single show - just display it as text
      Lucid.span_ [Lucid.class_ Tokens.textGray600] $ do
        Lucid.toHtml singleShow.title
    _ -> do
      -- Multiple shows - dropdown with JS redirect
      Lucid.select_
        [ Lucid.id_ "show-selector",
          Lucid.name_ "show",
          class_ $ base ["px-3", "py-1", "border", "border-gray-300", Tokens.bgWhite, Tokens.textSm, "font-medium"],
          onchange_ "window.location.href = this.options[this.selectedIndex].dataset.url"
        ]
        $ mapM_ (renderShowOption activeNav selectedShow) allShows

-- | Render a single show option with pre-generated URL using safe links
renderShowOption :: DashboardNav -> Maybe Shows.Model -> Shows.Model -> Lucid.Html ()
renderShowOption activeNav selectedShow showModel = do
  let isSelected = maybe False (\s -> Shows.slug s == Shows.slug showModel) selectedShow
      showSlug = Shows.slug showModel
      showTitle = Shows.title showModel
      -- Generate URL using safe links - stays on same nav but changes show
      url = navUrl activeNav (Just showModel)
      urlAttr = maybe "" (\u -> [i|/#{u}|]) url
  Lucid.option_
    ( [ Lucid.value_ (display showSlug),
        Lucid.data_ "url" urlAttr
      ]
        <> [Lucid.selected_ "selected" | isSelected]
    )
    (Lucid.toHtml showTitle)

-- | Navigation item - simple link with server-rendered active state
-- If no URL can be generated (e.g., Episodes without a show), renders as disabled
navItem :: Text -> DashboardNav -> DashboardNav -> Maybe Shows.Model -> Lucid.Html ()
navItem label nav activeNav mShow = do
  let isActive = nav == activeNav
      baseClasses = class_' $ base ["block", Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, "transition-colors"]
      activeClasses = class_' $ base [Tokens.bgGray800, Tokens.textWhite]
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
      baseClasses = class_' $ base ["block", Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, "transition-colors"]
      activeClasses = class_' $ base [Tokens.bgGray800, Tokens.textWhite]
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
  NavStationBlog -> Just dashboardStationBlogGetUrl
  NavEvents -> Just dashboardEventsGetUrl
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
        NavSettings -> dashboardShowSettingsUrl <$> mSlug
        NavUsers -> Just dashboardUsersGetUrl
        NavShows -> Just dashboardShowsGetUrl
        NavStationBlog -> Just dashboardStationBlogGetUrl
        NavEvents -> Just dashboardEventsGetUrl

-- | Top bar with page title, show selector, stats, action button, and back link
topBar :: DashboardNav -> [Shows.Model] -> Maybe Shows.Model -> Maybe (Lucid.Html ()) -> Maybe (Lucid.Html ()) -> Lucid.Html ()
topBar activeNav allShows selectedShow statsContent actionButton =
  Lucid.header_ [class_ $ base [Tokens.bgWhite, "border-b", "border-gray-200", Tokens.px8, Tokens.py4]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      -- Left side: page title and show selector
      Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4]] $ do
        Lucid.h1_ [class_ $ base [Tokens.textXl, Tokens.fontBold]] $
          Lucid.toHtml (navTitle activeNav)
        -- Show selector (only for show-scoped pages)
        when (isShowScoped activeNav) $
          showSelector activeNav allShows selectedShow
        -- Stats (schedule info, counts, etc.)
        case statsContent of
          Just stats -> Lucid.div_ [class_ $ base [Tokens.textSm, "text-gray-500", "border-l", "border-gray-300", "pl-4"]] stats
          Nothing -> mempty
      -- Right side: action button and back to site link
      Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4]] $ do
        fromMaybe mempty actionButton
        Lucid.a_
          [ Lucid.href_ [i|/#{rootGetUrl}|],
            class_ $ base [Tokens.textSm, "text-gray-500", "hover:text-gray-800"]
          ]
          "Back to Site"

--------------------------------------------------------------------------------

-- | Load dashboard frame (full page)
loadDashboardFrame ::
  (Log.MonadLog m, MonadThrow m) =>
  Maybe GoogleAnalyticsId ->
  UserMetadata.Model ->
  [Shows.Model] ->
  Maybe Shows.Model ->
  DashboardNav ->
  Maybe (Lucid.Html ()) ->
  Maybe (Lucid.Html ()) ->
  Lucid.Html () ->
  m (Lucid.Html ())
loadDashboardFrame mGoogleAnalyticsId user allShows selectedShow nav statsContent actionButton content =
  pure $ template mGoogleAnalyticsId user allShows selectedShow nav statsContent actionButton content

-- | Load content only for HTMX requests
loadDashboardContentOnly :: (Log.MonadLog m, MonadThrow m) => Lucid.Html () -> m (Lucid.Html ())
loadDashboardContentOnly = pure
