{-# LANGUAGE QuasiQuotes #-}

module Component.DashboardFrame
  ( template,
    loadDashboardFrame,
    htmxResponse,
    pageSwap,
    mainContentSwap,
    mainContentLoad,
    DashboardNav (..),
    sectionUrl,
  )
where

--------------------------------------------------------------------------------

import API.Links
  ( apiLinks,
    dashboardAnalyticsLinks,
    dashboardAnnouncementsLinks,
    dashboardBlogsLinks,
    dashboardEphemeralUploadsLinks,
    dashboardEpisodesLinks,
    dashboardEventsLinks,
    dashboardInvitationsLinks,
    dashboardLinks,
    dashboardMissingEpisodesLink,
    dashboardNewsletterSubscribersLinks,
    dashboardPromosLinks,
    dashboardPsasLinks,
    dashboardShowsLinks,
    dashboardSitePagesLinks,
    dashboardStationBlogLinks,
    dashboardStationIdsLinks,
    dashboardStoreOrdersLinks,
    dashboardStoreProductsLinks,
    dashboardStoreSettingsLinks,
    dashboardStreamSettingsLinks,
    dashboardTimelineLink,
    dashboardUnderwritersLinks,
    dashboardUnderwritingLinks,
    dashboardUsersLinks,
    rootLink,
    staticAssetLink,
    userLinks,
  )
import API.Types
import App.Dashboard.ShowContext (ShowContext (..), ShowSection (..), currentShowCookieName)
import Component.Banner (bannerContainerId)
import Component.Frame (bannerFromUrlScript, clearBannerOnNavScript, darkModeScript, googleAnalyticsScript, htmxErrorBannerScript, htmxIndicatorStyles, showBannerScript)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Lazy qualified as LText
import Design (base, class_, class_')
import Design.FormStyles (formBuilderCSS)
import Design.Theme (getTheme, themeCSS)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Domain.Types.GoogleAnalyticsId (GoogleAnalyticsId)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Lucid.Alpine
import Lucid.Base qualified as LucidBase
import Lucid.HTMX
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
dashboardShowsGetUrl = Links.linkURI $ dashboardShowsLinks.list Nothing Nothing Nothing False

dashboardStationBlogGetUrl :: Links.URI
dashboardStationBlogGetUrl = Links.linkURI $ dashboardStationBlogLinks.list Nothing

dashboardEventsGetUrl :: Links.URI
dashboardEventsGetUrl = Links.linkURI $ dashboardEventsLinks.list Nothing

dashboardShowSettingsUrl :: Slug -> Links.URI
dashboardShowSettingsUrl slug = Links.linkURI $ dashboardShowsLinks.editGet slug

dashboardStationIdsGetUrl :: Links.URI
dashboardStationIdsGetUrl = Links.linkURI $ dashboardStationIdsLinks.list Nothing

dashboardEphemeralUploadsGetUrl :: Links.URI
dashboardEphemeralUploadsGetUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.list Nothing

dashboardSitePagesGetUrl :: Links.URI
dashboardSitePagesGetUrl = Links.linkURI dashboardSitePagesLinks.list

dashboardStreamSettingsGetUrl :: Links.URI
dashboardStreamSettingsGetUrl = Links.linkURI dashboardStreamSettingsLinks.get

dashboardMissingEpisodesGetUrl :: Links.URI
dashboardMissingEpisodesGetUrl = Links.linkURI dashboardMissingEpisodesLink

dashboardAnalyticsGetUrl :: Links.URI
dashboardAnalyticsGetUrl = Links.linkURI $ dashboardAnalyticsLinks.get Nothing

dashboardInvitationsGetUrl :: Links.URI
dashboardInvitationsGetUrl = Links.linkURI dashboardInvitationsLinks.list

dashboardNewsletterSubscribersGetUrl :: Links.URI
dashboardNewsletterSubscribersGetUrl =
  Links.linkURI $ dashboardNewsletterSubscribersLinks.list Nothing Nothing

dashboardPsasGetUrl :: Links.URI
dashboardPsasGetUrl = Links.linkURI $ dashboardPsasLinks.list Nothing

dashboardAnnouncementsGetUrl :: Links.URI
dashboardAnnouncementsGetUrl = Links.linkURI $ dashboardAnnouncementsLinks.list Nothing

dashboardPromosGetUrl :: Links.URI
dashboardPromosGetUrl = Links.linkURI $ dashboardPromosLinks.list Nothing

dashboardShowPromosGetUrl :: Slug -> Links.URI
dashboardShowPromosGetUrl slug = Links.linkURI $ dashboardShowsLinks.promosGet slug

dashboardUnderwritingGetUrl :: Links.URI
dashboardUnderwritingGetUrl = Links.linkURI $ dashboardUnderwritingLinks.list Nothing

dashboardUnderwritersGetUrl :: Links.URI
dashboardUnderwritersGetUrl = Links.linkURI dashboardUnderwritersLinks.list

dashboardUnderwritingDeliveryUrl :: Links.URI
dashboardUnderwritingDeliveryUrl = Links.linkURI $ dashboardUnderwritingLinks.delivery Nothing

dashboardTimelineUrl :: Links.URI
dashboardTimelineUrl = Links.linkURI $ dashboardTimelineLink Nothing

dashboardStoreProductsGetUrl :: Links.URI
dashboardStoreProductsGetUrl = Links.linkURI dashboardStoreProductsLinks.list

dashboardStoreOrdersGetUrl :: Links.URI
dashboardStoreOrdersGetUrl = Links.linkURI dashboardStoreOrdersLinks.list

dashboardStoreSettingsGetUrl :: Links.URI
dashboardStoreSettingsGetUrl = Links.linkURI dashboardStoreSettingsLinks.get

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
  | NavStationIds
  | NavEphemeralUploads
  | NavSitePages
  | NavStreamSettings
  | NavMissingEpisodes
  | NavAnalytics
  | NavInvitations
  | NavNewsletterSubscribers
  | NavPsas
  | NavAnnouncements
  | NavPromos
  | NavShowPromos
  | NavUnderwriting
  | NavUnderwriters
  | NavUnderwritingDelivery
  | NavTimeline
  | NavStoreProducts
  | NavStoreOrders
  | NavStoreSettings
  deriving (Eq, Show)

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
  NavStationIds -> False
  NavEphemeralUploads -> False
  NavSitePages -> False
  NavStreamSettings -> False
  NavMissingEpisodes -> False
  NavAnalytics -> False
  NavInvitations -> False
  NavNewsletterSubscribers -> False
  NavPsas -> False
  NavAnnouncements -> False
  NavPromos -> False
  NavShowPromos -> True
  NavUnderwriting -> False
  NavUnderwriters -> False
  NavUnderwritingDelivery -> False
  NavTimeline -> False
  NavStoreProducts -> False
  NavStoreOrders -> False
  NavStoreSettings -> False

-- | Where a frame part goes.
data Placement
  = -- | In the frame of a full page.
    InFrame
  | -- | In an HTMX response. It replaces the copy in the frame.
    OutOfBand
  deriving stock (Show, Eq)

-- | Dashboard frame template - full page liquid layout with sidebar
template ::
  Maybe GoogleAnalyticsId ->
  UserMetadata.Model ->
  ShowContext ->
  -- | Options of the show selector. Not used with 'NoShow'.
  [Shows.Model] ->
  DashboardNav ->
  Maybe (Lucid.Html ()) ->
  Maybe (Lucid.Html ()) ->
  Lucid.Html () ->
  Lucid.Html ()
template mGoogleAnalyticsId userMeta showContext selectorShows activeNav statsContent actionButton main =
  Lucid.doctypehtml_ $ do
    Lucid.head_ $ do
      googleAnalyticsScript mGoogleAnalyticsId
      Lucid.title_ "Dashboard | KPBJ 95.9FM"
      -- The body has no HTMX history snapshot. On a back or forward step,
      -- HTMX loads the full page again.
      Lucid.meta_ [Lucid.name_ "htmx-config", Lucid.content_ "{\"refreshOnHistoryMiss\":true}"]
      -- Theme CSS (must be before Tailwind for CSS variable availability)
      let theme = getTheme userMeta.mTheme
      Lucid.style_ [] (themeCSS theme)
      -- HTMX indicator CSS (hides loading indicators until request in flight)
      Lucid.style_ [] htmxIndicatorStyles
      -- Form builder semantic classes
      Lucid.style_ [] formBuilderCSS
      Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.7.1/css/all.min.css"]
      Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ (rootLink $ staticAssetLink "cropper.min.css")]
      Lucid.script_ [Lucid.src_ "https://cdn.tailwindcss.com"] (mempty @Text)
      Lucid.script_ [Lucid.src_ (rootLink $ staticAssetLink "cropper.min.js")] (mempty @Text)
      Lucid.script_ [Lucid.src_ (rootLink $ staticAssetLink "htmx.min.js")] (mempty @Text)
      Lucid.script_ [Lucid.src_ (rootLink $ staticAssetLink "alpine.min.js"), Lucid.defer_ "true"] (mempty @Text)
      Lucid.script_ [] ("tailwind.config = { darkMode: 'class', theme: { fontFamily: { sans: ['ui-monospace', 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', 'Courier New', 'monospace'], mono: ['ui-monospace', 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', 'Courier New', 'monospace'] } } }" :: Text)
      Lucid.script_ [] (darkModeScript (Just userMeta.mColorScheme))
      -- Hide Alpine content that has x-cloak until Alpine starts.
      Lucid.style_ [] ("[x-cloak] { display: none !important; }" :: Text)
      Lucid.script_ [] showBannerScript
      Lucid.script_ [] htmxErrorBannerScript
      Lucid.script_ [] clearBannerOnNavScript
      Lucid.script_ [] abortOlderPageRequestScript
      Lucid.script_ [] removeHistoryCacheScript
      currentShowCookieTag showContext
    -- An HTMX history snapshot holds the DOM after Alpine changed it. Alpine
    -- fails when it starts on that DOM again, so HTMX saves no snapshot.
    -- The @dashboard@ class scopes the dashboard button styles in forms.css.
    Lucid.body_ [class_ $ base ["dashboard", "font-mono", Tokens.fgPrimary, Tokens.bgAlt], LucidBase.makeAttributes "hx-history" "false"] $ do
      -- Full height flex container
      Lucid.div_ [class_ $ base ["min-h-screen", "flex"]] $ do
        -- Left sidebar
        sidebar userMeta activeNav showContext
        -- Main content area
        Lucid.div_ [class_ $ base ["flex-1", "flex", "flex-col"]] $ do
          -- Top bar with page title, show selector, stats, and action button
          topBar InFrame activeNav selectorShows showContext statsContent actionButton
          -- Banner container
          Lucid.div_ [class_ $ base ["px-6", Tokens.mt4]] $
            Lucid.div_ [Lucid.id_ bannerContainerId, Lucid.class_ Tokens.fullWidth] mempty
          -- Main content (use same ID as main site for HTMX compatibility)
          Lucid.main_ [class_ $ base ["flex-1", "px-6", "py-4"], Lucid.id_ "main-content"] main
      -- Script to display banner from URL params (runs after DOM is ready)
      Lucid.script_ [] bannerFromUrlScript

-- | A @script@ element that remembers the show of the page as the current
-- show. A page that is not about a show gets nothing.
currentShowCookieTag :: ShowContext -> Lucid.Html ()
currentShowCookieTag = \case
  NoShow -> mempty
  ForShow currentShow -> Lucid.script_ [] (currentShowCookieScript currentShow.slug)

-- | A script that stores the slug in the current show cookie.
--
-- The slug goes into the script as a JSON string literal. The literal
-- escapes each @</@ so that the slug cannot close the @script@ element.
currentShowCookieScript :: Slug -> Text
currentShowCookieScript slug =
  [i|document.cookie = "#{currentShowCookieName}=" + encodeURIComponent(#{slugLiteral}) + "; Path=/dashboard; Max-Age=31536000; SameSite=Lax";|]
  where
    jsonLiteral = LText.toStrict $ Aeson.encodeToLazyText (Aeson.String (display slug))
    slugLiteral = Text.replace "</" "<\\/" jsonLiteral

-- | Left sidebar with navigation
sidebar ::
  UserMetadata.Model ->
  DashboardNav ->
  ShowContext ->
  Lucid.Html ()
sidebar userMeta activeNav showContext =
  Lucid.aside_ [class_ $ base ["w-64", Theme.bgMain, Tokens.fgInverse, "flex", "flex-col", "h-screen", "sticky", "top-0"]] $ do
    -- Logo / Brand (h-16 = 64px to align with top bar)
    Lucid.div_ [class_ $ base [Tokens.p4, "border-b", Theme.borderMuted, "shrink-0", "h-16", "flex", "flex-col", "justify-center"]] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{rootGetUrl}|],
          class_ $ base [Tokens.textLg, Theme.fgPrimary, Tokens.fontBold, Theme.hoverBg, "block"]
        ]
        "KPBJ 95.9FM"
      Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.fgMuted, "block", "mt-1"]] "Dashboard"

    sidebarNav InFrame userMeta activeNav showContext
    sidebarUser userMeta

-- | The navigation of the sidebar. It scrolls when the window is short.
--
-- The out-of-band form replaces only the children of the element in the
-- frame. The element stays, so the sidebar keeps its scroll position.
sidebarNav ::
  Placement ->
  UserMetadata.Model ->
  DashboardNav ->
  ShowContext ->
  Lucid.Html ()
sidebarNav placement userMeta activeNav showContext =
  Lucid.nav_
    ( [ Lucid.id_ "dashboard-nav",
        class_ $ base ["flex-1", Tokens.p4, "overflow-y-auto"]
      ]
        <> [hxSwapOob_ "innerHTML" | placement == OutOfBand]
    )
    $ do
      Lucid.ul_ [Lucid.class_ "space-y-2"] $ do
        Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.fgMuted, "block", Tokens.px4, Tokens.mb2]] "HOST"
        Lucid.ul_ [Lucid.class_ "space-y-2"] $ do
          navItem "EPISODES" NavEpisodes activeNav (hostNavUrl SectionEpisodes showContext)
          navItem "PROMOS" NavShowPromos activeNav (hostNavUrl SectionPromos showContext)
          navItem "SHOW SETTINGS" NavSettings activeNav (hostNavUrl SectionSettings showContext)
          -- Staff find station IDs under AIR BREAKS, so each user sees it once.
          unless (UserMetadata.isStaffOrHigher userMeta.mUserRole) $
            staffNavItem "STATION IDS" NavStationIds activeNav
          staffNavItem "EPHEMERAL" NavEphemeralUploads activeNav

      -- Staff/Admin section - shown only for Staff or higher roles
      when (UserMetadata.isStaffOrHigher userMeta.mUserRole) $ do
        Lucid.div_ [class_ $ base ["border-t", Theme.borderMuted, "mt-4", "pt-4"]] $ do
          Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.fgMuted, "block", Tokens.px4, Tokens.mb2]] "ADMIN"
          Lucid.ul_ [Lucid.class_ "space-y-2"] $ do
            staffNavItem "USERS" NavUsers activeNav
            staffNavItem "INVITATIONS" NavInvitations activeNav
            staffNavItem "NEWSLETTER" NavNewsletterSubscribers activeNav
            staffNavItem "SHOWS" NavShows activeNav
            staffNavItem "STATION BLOG" NavStationBlog activeNav
            staffNavItem "EVENTS" NavEvents activeNav
            staffNavItem "MISSING EPISODES" NavMissingEpisodes activeNav
            staffNavItem "TIMELINE" NavTimeline activeNav
            staffNavItem "SITE PAGES" NavSitePages activeNav
            -- Admin-only items
            when (UserMetadata.isAdmin userMeta.mUserRole) $ do
              staffNavItem "STREAM" NavStreamSettings activeNav
              staffNavItem "ANALYTICS" NavAnalytics activeNav
        Lucid.div_ [class_ $ base ["border-t", Theme.borderMuted, "mt-4", "pt-4"]] $ do
          Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.fgMuted, "block", Tokens.px4, Tokens.mb2]] "AIR BREAKS"
          Lucid.ul_ [Lucid.class_ "space-y-2"] $ do
            staffNavItem "STATION IDS" NavStationIds activeNav
            staffNavItem "PSAS" NavPsas activeNav
            staffNavItem "ANNOUNCEMENTS" NavAnnouncements activeNav
            staffNavItem "PROMOS" NavPromos activeNav
            staffNavItem "UNDERWRITING" NavUnderwriting activeNav
            staffNavItem "UNDERWRITERS" NavUnderwriters activeNav
            staffNavItem "DELIVERY" NavUnderwritingDelivery activeNav
        Lucid.div_ [class_ $ base ["border-t", Theme.borderMuted, "mt-4", "pt-4"]] $ do
          Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.fgMuted, "block", Tokens.px4, Tokens.mb2]] "STORE"
          Lucid.ul_ [Lucid.class_ "space-y-2"] $ do
            staffNavItem "PRODUCTS" NavStoreProducts activeNav
            staffNavItem "ORDERS" NavStoreOrders activeNav
            when (UserMetadata.isAdmin userMeta.mUserRole) $
              staffNavItem "SETTINGS" NavStoreSettings activeNav

-- | The user name, the role, and the account links at the bottom of the
-- sidebar. They are always visible.
sidebarUser :: UserMetadata.Model -> Lucid.Html ()
sidebarUser userMeta =
  Lucid.div_ [class_ $ base [Tokens.p4, "border-t", Theme.borderMuted, "shrink-0", "mt-auto"]] $ do
    Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted, Tokens.mb2]] $ do
      Lucid.span_ [class_ $ base ["block", Tokens.fontBold, Tokens.fgInverse]] $
        Lucid.toHtml userMeta.mDisplayName
      Lucid.span_ [Lucid.class_ Tokens.textXs] $
        Lucid.toHtml $
          Text.pack $
            show userMeta.mUserRole
    -- Edit Profile and Logout buttons
    Lucid.div_ [class_ $ base ["flex", "flex-col", Tokens.gap4, "mt-2"]] $ do
      Lucid.a_
        ( [Lucid.href_ [i|/#{dashboardProfileEditUrl}|], class_ $ base [Tokens.textSm, Tokens.fgMuted, "hover:text-[var(--theme-fg-inverse)]"]]
            <> mainContentSwap dashboardProfileEditUrl
        )
        "Settings"
      Lucid.a_
        [Lucid.href_ [i|/#{userLogoutGetUrl}|], class_ $ base [Tokens.textSm, Tokens.fgMuted, "hover:text-[var(--theme-fg-inverse)]"], hxGet_ [i|/#{userLogoutGetUrl}|]]
        "Logout"

-- | Show selector dropdown for top bar
--
-- A change loads the same section for the selected show. The request is an
-- HTMX request with the selector as its source, so HTMX reads the swap target
-- and the history setting from the attributes of the selector.
--
-- Staff can open a show that they do not host. The selector then gets that
-- show as its first option, so it always names the show of the page.
showSelector :: DashboardNav -> [Shows.Model] -> Shows.Model -> Lucid.Html ()
showSelector activeNav selectorShows selectedShow =
  case selectorOptions of
    [] -> mempty
    [singleShow] ->
      -- Single show - just display it as text
      Lucid.span_ [Lucid.class_ Tokens.fgMuted] $ do
        Lucid.toHtml singleShow.title
    _ -> do
      -- Multiple shows - dropdown that swaps the main content
      -- The selector has no name. HTMX adds the value of a named source
      -- element to the request URL.
      Lucid.select_
        [ Lucid.id_ "show-selector",
          class_ $ base ["px-3", "py-1", "border", Tokens.borderDefault, Tokens.bgInverse, Tokens.fgInverse, Tokens.textSm, "font-medium"],
          hxTarget_ "#main-content",
          hxSwap_ pageSwap,
          hxPushUrl_ "true",
          -- The browser changes the value before the request. If the request
          -- does not succeed, go back to the show of the page on screen.
          hxOnAfterRequest_ "if (!event.detail.successful) this.value = this.querySelector('option[selected]').value",
          onchange_ "htmx.ajax('GET', this.options[this.selectedIndex].dataset.url, {source: this})"
        ]
        $ mapM_ (renderShowOption activeNav selectedShow) selectorOptions
  where
    selectorOptions
      | any (\s -> s.slug == selectedShow.slug) selectorShows = selectorShows
      | otherwise = selectedShow : selectorShows

-- | Render a single show option with pre-generated URL using safe links
renderShowOption :: DashboardNav -> Shows.Model -> Shows.Model -> Lucid.Html ()
renderShowOption activeNav selectedShow showModel = do
  let isSelected = Shows.slug selectedShow == Shows.slug showModel
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

-- | Navigation item with server-rendered active state
navItem :: Text -> DashboardNav -> DashboardNav -> Links.URI -> Lucid.Html ()
navItem label nav activeNav = navLink label (nav == activeNav)

-- | A sidebar link. It swaps the main content and pushes the URL.
--
-- The HTMX response brings the sidebar navigation out of band, so the active
-- item is correct after the swap.
navLink ::
  -- | Label
  Text ->
  -- | Is this the active item?
  Bool ->
  Links.URI ->
  Lucid.Html ()
navLink label isActive url =
  Lucid.li_ $
    Lucid.a_
      ([Lucid.href_ [i|/#{url}|], Lucid.class_ classes] <> mainContentSwap url)
      (Lucid.toHtml label)
  where
    baseClasses = class_' $ base ["block", Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, "transition-colors"]
    activeClasses = class_' $ base [Tokens.bgInverse, Tokens.fgInverse]
    inactiveClasses = class_' $ base [Tokens.fgMuted, "hover:text-[var(--theme-fg-inverse)]", Tokens.hoverBg]
    classes = baseClasses <> " " <> if isActive then activeClasses else inactiveClasses

-- | The HTMX attributes of a link that swaps the main content and pushes the
-- URL to the browser history. The new page starts at the top of the window.
mainContentSwap :: Links.URI -> [Lucid.Attributes]
mainContentSwap url = mainContentLoad url <> [hxPushUrl_ "true"]

-- | The HTMX attributes of a link that swaps the main content and does not
-- change the browser history. Use it for a link to the page on screen, for
-- example a refresh link. The new page starts at the top of the window.
mainContentLoad :: Links.URI -> [Lucid.Attributes]
mainContentLoad url =
  [ hxGet_ [i|/#{url}|],
    hxTarget_ "#main-content",
    hxSwap_ pageSwap
  ]

-- | JavaScript that removes the HTMX history cache from the browser.
--
-- The dashboard saves no snapshot, but an older version of the dashboard did.
-- HTMX restores a snapshot that it finds, and the page then has old HTML. The
-- public site shares the cache, so it also loses its snapshots. It gets those
-- pages from the server on a back step.
removeHistoryCacheScript :: Text
removeHistoryCacheScript =
  "try { localStorage.removeItem('htmx-history-cache'); } catch (e) {}"

-- | JavaScript that aborts an older page request when a new one starts.
--
-- A page request is a GET request with the target @#main-content@. Without
-- this script, a slow response can replace a newer page. The script covers
-- each element that makes a page request, and the request of an
-- @HX-Location@ header, so no element needs @hx-sync@.
abortOlderPageRequestScript :: Text
abortOlderPageRequestScript =
  [i|
    (function() {
      let pageXhr = null;
      document.addEventListener('htmx:beforeSend', function(event) {
        const detail = event.detail;
        if (detail.requestConfig.verb !== 'get') return;
        if (!detail.target || detail.target.id !== 'main-content') return;
        if (pageXhr) pageXhr.abort();
        pageXhr = detail.xhr;
      });
      document.addEventListener('htmx:afterRequest', function(event) {
        if (event.detail.xhr === pageXhr) pageXhr = null;
      });
    })();
  |]

-- | The swap specification of a page change from the sidebar. The new page
-- starts at the top of the window.
pageSwap :: Text
pageSwap = "innerHTML show:window:top"

-- | URL of a HOST navigation item.
--
-- A page for a show links to the section of that show. A page that is not
-- about a show links to the route that finds the current show.
hostNavUrl :: ShowSection -> ShowContext -> Links.URI
hostNavUrl section = \case
  NoShow -> Links.linkURI $ dashboardLinks.currentShow section
  ForShow currentShow -> sectionUrl section currentShow

-- | The page of a section for one show.
sectionUrl :: ShowSection -> Shows.Model -> Links.URI
sectionUrl section showModel = case section of
  SectionEpisodes -> dashboardEpisodesGetUrl showModel.slug
  SectionPromos -> dashboardShowPromosGetUrl showModel.slug
  SectionSettings -> dashboardShowSettingsUrl showModel.slug

-- | Staff navigation item. It does not need a show.
staffNavItem :: Text -> DashboardNav -> DashboardNav -> Lucid.Html ()
staffNavItem label nav activeNav =
  foldMap (navLink label (nav == activeNav)) (staffNavUrl nav)

-- | Get URL for staff navigation item (no show context)
staffNavUrl :: DashboardNav -> Maybe Links.URI
staffNavUrl = \case
  NavUsers -> Just dashboardUsersGetUrl
  NavShows -> Just dashboardShowsGetUrl
  NavStationBlog -> Just dashboardStationBlogGetUrl
  NavEvents -> Just dashboardEventsGetUrl
  NavStationIds -> Just dashboardStationIdsGetUrl
  NavEphemeralUploads -> Just dashboardEphemeralUploadsGetUrl
  NavSitePages -> Just dashboardSitePagesGetUrl
  NavStreamSettings -> Just dashboardStreamSettingsGetUrl
  NavMissingEpisodes -> Just dashboardMissingEpisodesGetUrl
  NavAnalytics -> Just dashboardAnalyticsGetUrl
  NavInvitations -> Just dashboardInvitationsGetUrl
  NavNewsletterSubscribers -> Just dashboardNewsletterSubscribersGetUrl
  NavPsas -> Just dashboardPsasGetUrl
  NavAnnouncements -> Just dashboardAnnouncementsGetUrl
  NavPromos -> Just dashboardPromosGetUrl
  NavUnderwriting -> Just dashboardUnderwritingGetUrl
  NavUnderwriters -> Just dashboardUnderwritersGetUrl
  NavUnderwritingDelivery -> Just dashboardUnderwritingDeliveryUrl
  NavTimeline -> Just dashboardTimelineUrl
  NavStoreProducts -> Just dashboardStoreProductsGetUrl
  NavStoreOrders -> Just dashboardStoreOrdersGetUrl
  NavStoreSettings -> Just dashboardStoreSettingsGetUrl
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
        NavStationIds -> Just dashboardStationIdsGetUrl
        NavEphemeralUploads -> Just dashboardEphemeralUploadsGetUrl
        NavSitePages -> Just dashboardSitePagesGetUrl
        NavStreamSettings -> Just dashboardStreamSettingsGetUrl
        NavMissingEpisodes -> Just dashboardMissingEpisodesGetUrl
        NavAnalytics -> Just dashboardAnalyticsGetUrl
        NavInvitations -> Just dashboardInvitationsGetUrl
        NavNewsletterSubscribers -> Just dashboardNewsletterSubscribersGetUrl
        NavPsas -> Just dashboardPsasGetUrl
        NavAnnouncements -> Just dashboardAnnouncementsGetUrl
        NavPromos -> Just dashboardPromosGetUrl
        NavShowPromos -> dashboardShowPromosGetUrl <$> mSlug
        NavUnderwriting -> Just dashboardUnderwritingGetUrl
        NavUnderwriters -> Just dashboardUnderwritersGetUrl
        NavUnderwritingDelivery -> Just dashboardUnderwritingDeliveryUrl
        NavTimeline -> Just dashboardTimelineUrl
        NavStoreProducts -> Just dashboardStoreProductsGetUrl
        NavStoreOrders -> Just dashboardStoreOrdersGetUrl
        NavStoreSettings -> Just dashboardStoreSettingsGetUrl

-- | Top bar with show selector, stats, action button, and back link
topBar :: Placement -> DashboardNav -> [Shows.Model] -> ShowContext -> Maybe (Lucid.Html ()) -> Maybe (Lucid.Html ()) -> Lucid.Html ()
topBar placement activeNav selectorShows showContext statsContent actionButton =
  Lucid.header_
    ( [ Lucid.id_ "dashboard-top-bar",
        class_ $ base [Tokens.bgMain, Tokens.px8, "sticky", "top-0", "z-10", "border-b", Theme.borderMuted, "h-16", "flex", "items-center"]
      ]
        <> [hxSwapOob_ "true" | placement == OutOfBand]
    )
    $ do
      Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between", "w-full"]] $ do
        -- Left side: show selector and stats
        Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4]] $ do
          -- Show selector (only for show-scoped pages that have a show)
          case showContext of
            ForShow currentShow
              | isShowScoped activeNav -> showSelector activeNav selectorShows currentShow
            _ -> mempty
          -- Stats (schedule info, counts, etc.)
          case statsContent of
            Just stats -> Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted, "border-l", Tokens.borderMuted, "pl-4"]] stats
            Nothing -> mempty
        -- Right side: action button and back to site link
        Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4]] $ do
          fromMaybe mempty actionButton
          Lucid.a_
            [ Lucid.href_ [i|/#{rootGetUrl}|],
              class_ $ base [Tokens.textSm, Tokens.fgMuted, "hover:text-[var(--theme-fg)]"]
            ]
            "Back to Site"

--------------------------------------------------------------------------------

-- | Load dashboard frame (full page)
loadDashboardFrame ::
  (Log.MonadLog m, MonadThrow m) =>
  Maybe GoogleAnalyticsId ->
  UserMetadata.Model ->
  ShowContext ->
  -- | Options of the show selector. Not used with 'NoShow'.
  [Shows.Model] ->
  DashboardNav ->
  Maybe (Lucid.Html ()) ->
  Maybe (Lucid.Html ()) ->
  Lucid.Html () ->
  m (Lucid.Html ())
loadDashboardFrame mGoogleAnalyticsId user showContext selectorShows nav statsContent actionButton content =
  pure $ template mGoogleAnalyticsId user showContext selectorShows nav statsContent actionButton content

-- | The response to an HTMX request for a dashboard page.
--
-- HTMX swaps the content into the main element. The document head stays as
-- it is, so the content of a show page brings the cookie script with it.
-- HTMX runs the script after the swap.
--
-- The sidebar navigation and the top bar go out of band. HTMX puts each one
-- in place of its copy in the frame, so the frame describes the new page.
htmxResponse ::
  UserMetadata.Model ->
  ShowContext ->
  -- | Options of the show selector. Not used with 'NoShow'.
  [Shows.Model] ->
  DashboardNav ->
  Maybe (Lucid.Html ()) ->
  Maybe (Lucid.Html ()) ->
  Lucid.Html () ->
  Lucid.Html ()
htmxResponse userMeta showContext selectorShows activeNav statsContent actionButton content = do
  currentShowCookieTag showContext
  content
  sidebarNav OutOfBand userMeta activeNav showContext
  topBar OutOfBand activeNav selectorShows showContext statsContent actionButton
