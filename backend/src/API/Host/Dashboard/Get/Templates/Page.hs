{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Host.Dashboard.Get.Templates.Page
  ( template,
  )
where

import {-# SOURCE #-} API (episodesNewGetLink)
import API.Host.Dashboard.Get.Templates.BlogPost (renderBlogPostCard)
import API.Host.Dashboard.Get.Templates.Episode (renderEpisodeCard)
import API.Host.Dashboard.Get.Templates.Schedule (renderScheduleSection)
import API.Host.Dashboard.Get.Templates.Stats (renderStatsSection)
import Data.String.Interpolate (i)
import Data.Text.Display (display)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xData_, xModel_, xOnChange_)
import Servant.Links qualified as Links

-- | Host Dashboard template
template :: UserMetadata.Model -> [Shows.Model] -> Maybe Shows.Model -> [Episodes.Model] -> [ShowBlogPosts.Model] -> Lucid.Html ()
template userMeta allShows selectedShow recentEpisodes blogPosts = do
  -- Error banner container (empty by default, populated by HTMX out-of-band swaps)
  Lucid.div_ [Lucid.id_ "error-banner-container"] ""
  renderShowSelector allShows selectedShow
  renderDashboardContent userMeta selectedShow recentEpisodes blogPosts

-- | Show selector dropdown (only rendered when user has multiple shows)
renderShowSelector :: [Shows.Model] -> Maybe Shows.Model -> Lucid.Html ()
renderShowSelector allShows selectedShow =
  case allShows of
    [] -> mempty
    [_] -> mempty -- Single show, no need for selector
    _ -> do
      let selectedSlug = maybe "" (display . Shows.slug) selectedShow
      Lucid.section_
        [ Lucid.class_ "bg-gray-100 border-2 border-gray-800 p-4 mb-6 w-full",
          xData_ [i|{ selectedShow: '#{selectedSlug}' }|]
        ]
        $ do
          Lucid.label_ [Lucid.for_ "show-selector", Lucid.class_ "block font-bold mb-2"] "Select Show:"
          Lucid.select_
            [ Lucid.id_ "show-selector",
              Lucid.name_ "show",
              Lucid.class_ "w-full p-2 border-2 border-gray-800 font-bold bg-white",
              xModel_ "selectedShow",
              xOnChange_ "htmx.ajax('GET', '/host/dashboard?show=' + selectedShow, {target: '#main-content', swap: 'innerHTML', pushUrl: true})"
            ]
            $ mapM_ (renderShowOption selectedShow) allShows

-- | Render a single show option in the dropdown
renderShowOption :: Maybe Shows.Model -> Shows.Model -> Lucid.Html ()
renderShowOption selectedShow showModel = do
  let isSelected = maybe False (\s -> Shows.slug s == Shows.slug showModel) selectedShow
      showSlug = Shows.slug showModel
      showTitle = Shows.title showModel
  if isSelected
    then Lucid.option_ [Lucid.value_ (display showSlug), Lucid.selected_ "selected"] (Lucid.toHtml showTitle)
    else Lucid.option_ [Lucid.value_ (display showSlug)] (Lucid.toHtml showTitle)

-- | Render the main dashboard content
renderDashboardContent :: UserMetadata.Model -> Maybe Shows.Model -> [Episodes.Model] -> [ShowBlogPosts.Model] -> Lucid.Html ()
renderDashboardContent userMeta selectedShow recentEpisodes blogPosts = do
  renderDashboardHeader userMeta selectedShow
  renderQuickActions selectedShow
  renderDashboardGrid selectedShow recentEpisodes blogPosts

-- | Dashboard header with show info
renderDashboardHeader :: UserMetadata.Model -> Maybe Shows.Model -> Lucid.Html ()
renderDashboardHeader userMeta selectedShow =
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      renderHeaderInfo userMeta selectedShow
      renderShowIcon

-- | Header info (title, show, host, schedule, genre)
renderHeaderInfo :: UserMetadata.Model -> Maybe Shows.Model -> Lucid.Html ()
renderHeaderInfo userMeta selectedShow =
  Lucid.div_ $ do
    Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "HOST DASHBOARD"
    Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
      Lucid.strong_ "Show: "
      maybe mempty (Lucid.toHtml . (.title)) selectedShow
      " • "
      Lucid.strong_ "Host: "
      Lucid.toHtml userMeta.mDisplayName
    Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
      Lucid.strong_ "Schedule: "
      Lucid.span_ "TBD" -- TODO: Add schedule info
      " • "
      Lucid.strong_ "Genre: "
      maybe "TBD" (maybe mempty Lucid.toHtml . (.genre)) selectedShow

-- | Show icon and public page link
renderShowIcon :: Lucid.Html ()
renderShowIcon =
  Lucid.div_ [Lucid.class_ "text-center"] $ do
    Lucid.div_ [Lucid.class_ "w-16 h-16 bg-gray-300 mx-auto mb-2 flex items-center justify-center border-2 border-gray-600"] $
      Lucid.span_ [Lucid.class_ "text-2xl"] "🎵"
    -- TODO: Add link to show profile page when ready
    Lucid.span_ [Lucid.class_ "text-blue-300 text-sm"] "VIEW PUBLIC PAGE"

-- | Quick action buttons
renderQuickActions :: Maybe Shows.Model -> Lucid.Html ()
renderQuickActions selectedShow =
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8 w-full"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "QUICK ACTIONS"
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-3 gap-4"] $ do
      renderPrepareShowButton selectedShow
      renderNewBlogPostButton
      renderEditProfileButton

-- | Prepare Show button (disabled if no show selected)
renderPrepareShowButton :: Maybe Shows.Model -> Lucid.Html ()
renderPrepareShowButton selectedShow =
  case selectedShow of
    Just showModel -> do
      let uploadUrl = Links.linkURI $ episodesNewGetLink showModel.slug
      Lucid.a_
        [ Lucid.href_ [i|/#{uploadUrl}|],
          hxGet_ [i|/#{uploadUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-blue-600 text-white p-4 font-bold hover:bg-blue-700 transition-colors block text-center"
        ]
        "🎵 PREPARE SHOW"
    Nothing ->
      Lucid.div_
        [Lucid.class_ "bg-gray-400 text-white p-4 font-bold text-center opacity-50 cursor-not-allowed"]
        "🎵 PREPARE SHOW (No show assigned)"

-- | New Blog Post button
renderNewBlogPostButton :: Lucid.Html ()
renderNewBlogPostButton =
  Lucid.a_
    [ Lucid.href_ "#new-blog-post",
      Lucid.class_ "bg-green-600 text-white p-4 font-bold hover:bg-green-700 transition-colors block text-center"
    ]
    "📝 NEW BLOG POST"

-- | Edit Profile button
renderEditProfileButton :: Lucid.Html ()
renderEditProfileButton =
  Lucid.a_
    [ Lucid.href_ "#edit-profile",
      Lucid.class_ "bg-purple-600 text-white p-4 font-bold hover:bg-purple-700 transition-colors block text-center"
    ]
    "✏️ EDIT PROFILE"

-- | Main dashboard grid with episodes, blog posts, and sidebar
renderDashboardGrid :: Maybe Shows.Model -> [Episodes.Model] -> [ShowBlogPosts.Model] -> Lucid.Html ()
renderDashboardGrid selectedShow recentEpisodes blogPosts =
  Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-3 gap-8 mb-8 w-full"] $ do
    renderMainContent selectedShow recentEpisodes blogPosts
    renderSidebar selectedShow recentEpisodes blogPosts

-- | Main content area (episodes and blog posts)
renderMainContent :: Maybe Shows.Model -> [Episodes.Model] -> [ShowBlogPosts.Model] -> Lucid.Html ()
renderMainContent selectedShow recentEpisodes blogPosts =
  Lucid.div_ [Lucid.class_ "lg:col-span-2"] $ do
    renderRecentEpisodesSection selectedShow recentEpisodes
    renderRecentBlogPostsSection blogPosts

-- | Recent episodes section
renderRecentEpisodesSection :: Maybe Shows.Model -> [Episodes.Model] -> Lucid.Html ()
renderRecentEpisodesSection selectedShow recentEpisodes =
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "RECENT EPISODES"
    case recentEpisodes of
      [] ->
        Lucid.div_ [Lucid.class_ "text-gray-600 text-center p-8"] $ do
          Lucid.p_ "No episodes uploaded yet."
          Lucid.p_ [Lucid.class_ "text-sm mt-2"] "Use 'PREPARE SHOW' to upload your first episode."
      _ ->
        Lucid.div_ [Lucid.class_ "space-y-4"] $
          mapM_ (maybe (const mempty) renderEpisodeCard selectedShow) $
            take 3 recentEpisodes

-- | Recent blog posts section
renderRecentBlogPostsSection :: [ShowBlogPosts.Model] -> Lucid.Html ()
renderRecentBlogPostsSection blogPosts =
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex justify-between items-center mb-4"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold"] "RECENT BLOG POSTS"
      Lucid.button_
        [Lucid.class_ "bg-green-600 text-white px-4 py-2 text-sm font-bold hover:bg-green-700"]
        "NEW POST"
    case blogPosts of
      [] ->
        Lucid.div_ [Lucid.class_ "text-gray-600 text-center p-8"] $ do
          Lucid.p_ "No blog posts published yet."
          Lucid.p_ [Lucid.class_ "text-sm mt-2"] "Share your thoughts with your audience!"
      _ ->
        Lucid.div_ [Lucid.class_ "space-y-4"] $
          mapM_ renderBlogPostCard $
            take 3 blogPosts

-- | Sidebar with stats and schedule
renderSidebar :: Maybe Shows.Model -> [Episodes.Model] -> [ShowBlogPosts.Model] -> Lucid.Html ()
renderSidebar selectedShow recentEpisodes blogPosts =
  Lucid.div_ [Lucid.class_ "space-y-6"] $ do
    renderStatsSection recentEpisodes blogPosts
    maybe mempty renderScheduleSection selectedShow
