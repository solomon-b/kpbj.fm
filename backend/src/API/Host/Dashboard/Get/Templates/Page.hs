{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Host.Dashboard.Get.Templates.Page
  ( template,
  )
where

import {-# SOURCE #-} API (blogNewGetLink, episodesNewGetLink, showBlogNewGetLink, showEditGetLink, showGetLink)
import API.Host.Dashboard.Get.Templates.BlogPost (renderBlogPostTableRow)
import API.Host.Dashboard.Get.Templates.Episode (renderEpisodeTableRow)
import Component.Table (ColumnAlign (..), ColumnHeader (..), TableConfig (..), renderTable)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xBindClass_, xData_, xModel_, xOnChange_, xOnClick_, xShow_)
import Servant.Links qualified as Links

-- | Host Dashboard template
template :: UserMetadata.Model -> [Shows.Model] -> Maybe Shows.Model -> [Episodes.Model] -> [ShowBlogPosts.Model] -> [ShowSchedule.Model] -> Maybe ShowSchedule.UpcomingShowDate -> Lucid.Html ()
template userMeta allShows selectedShow recentEpisodes blogPosts schedules nextShow = do
  -- Error banner container (empty by default, populated by HTMX out-of-band swaps)
  Lucid.div_ [Lucid.id_ "error-banner-container"] ""
  renderShowSelector allShows selectedShow
  renderDashboardContent userMeta selectedShow recentEpisodes blogPosts schedules nextShow

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
renderDashboardContent :: UserMetadata.Model -> Maybe Shows.Model -> [Episodes.Model] -> [ShowBlogPosts.Model] -> [ShowSchedule.Model] -> Maybe ShowSchedule.UpcomingShowDate -> Lucid.Html ()
renderDashboardContent userMeta selectedShow recentEpisodes blogPosts schedules nextShow = do
  renderDashboardHeader userMeta selectedShow recentEpisodes blogPosts schedules nextShow
  renderContentTabs selectedShow recentEpisodes blogPosts

-- | Dashboard header with show info, stats, and schedule
renderDashboardHeader :: UserMetadata.Model -> Maybe Shows.Model -> [Episodes.Model] -> [ShowBlogPosts.Model] -> [ShowSchedule.Model] -> Maybe ShowSchedule.UpcomingShowDate -> Lucid.Html ()
renderDashboardHeader userMeta selectedShow recentEpisodes blogPosts schedules nextShow =
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      renderHeaderInfo userMeta selectedShow schedules nextShow recentEpisodes blogPosts
      renderShowIcon selectedShow

-- | Header info (title, show, host, schedule, stats)
renderHeaderInfo :: UserMetadata.Model -> Maybe Shows.Model -> [ShowSchedule.Model] -> Maybe ShowSchedule.UpcomingShowDate -> [Episodes.Model] -> [ShowBlogPosts.Model] -> Lucid.Html ()
renderHeaderInfo userMeta selectedShow schedules nextShow recentEpisodes blogPosts =
  Lucid.div_ $ do
    Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "HOST DASHBOARD"
    Lucid.div_ [Lucid.class_ "text-gray-300 text-sm mb-1"] $ do
      Lucid.strong_ "Show: "
      maybe mempty (Lucid.toHtml . (.title)) selectedShow
      " • "
      Lucid.strong_ "Host: "
      Lucid.toHtml userMeta.mDisplayName
    Lucid.div_ [Lucid.class_ "text-gray-300 text-sm mb-2"] $ do
      Lucid.strong_ "Schedule: "
      renderScheduleInfo schedules
      case nextShow of
        Just upcoming -> do
          " • "
          Lucid.strong_ "Next Show: "
          Lucid.toHtml $ Text.pack $ formatTime defaultTimeLocale "%b %d, %Y" (ShowSchedule.usdShowDate upcoming)
        Nothing -> mempty
    -- Stats row
    Lucid.div_ [Lucid.class_ "flex gap-6 text-sm mt-2"] $ do
      Lucid.div_ $ do
        Lucid.strong_ [Lucid.class_ "text-gray-400"] "Episodes: "
        Lucid.span_ [Lucid.class_ "text-white"] $ Lucid.toHtml $ show $ length recentEpisodes
      Lucid.div_ $ do
        Lucid.strong_ [Lucid.class_ "text-gray-400"] "Blog Posts: "
        Lucid.span_ [Lucid.class_ "text-white"] $ Lucid.toHtml $ show $ length blogPosts
      Lucid.div_ $ do
        Lucid.strong_ [Lucid.class_ "text-gray-400"] "Total Downloads: "
        Lucid.span_ [Lucid.class_ "text-white"] "TBD"

-- | Format schedule info from schedule models
renderScheduleInfo :: [ShowSchedule.Model] -> Lucid.Html ()
renderScheduleInfo [] = "Not scheduled"
renderScheduleInfo (firstSchedule : rest) =
  let allSchedules = firstSchedule : rest
      dayNames = map (dayOfWeekName . ShowSchedule.dayOfWeek) allSchedules
      dayText = Text.intercalate ", " dayNames
      timeRange = ShowSchedule.startTime firstSchedule <> " - " <> ShowSchedule.endTime firstSchedule
   in Lucid.toHtml $ dayText <> " • " <> timeRange

-- | Convert day of week number to name
dayOfWeekName :: Int64 -> Text
dayOfWeekName 0 = "Sun"
dayOfWeekName 1 = "Mon"
dayOfWeekName 2 = "Tue"
dayOfWeekName 3 = "Wed"
dayOfWeekName 4 = "Thu"
dayOfWeekName 5 = "Fri"
dayOfWeekName 6 = "Sat"
dayOfWeekName _ = "?"

-- | Show icon with public page link and edit button
renderShowIcon :: Maybe Shows.Model -> Lucid.Html ()
renderShowIcon selectedShow =
  Lucid.div_ [Lucid.class_ "text-center"] $ do
    Lucid.div_ [Lucid.class_ "w-16 h-16 bg-gray-300 mx-auto mb-2 flex items-center justify-center border-2 border-gray-600"] $
      Lucid.span_ [Lucid.class_ "text-2xl"] "🎵"
    case selectedShow of
      Just showModel -> do
        let showUrl = Links.linkURI $ showGetLink showModel.slug
            editUrl = Links.linkURI $ showEditGetLink showModel.slug
        Lucid.div_ [Lucid.class_ "flex flex-col gap-2"] $ do
          Lucid.a_
            [ Lucid.href_ [i|/#{showUrl}|],
              hxGet_ [i|/#{showUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
            ]
            "VIEW PUBLIC PAGE"
          Lucid.a_
            [ Lucid.href_ [i|/#{editUrl}|],
              hxGet_ [i|/#{editUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "bg-purple-600 text-white px-4 py-2 text-xs font-bold hover:bg-purple-700 no-underline inline-block"
            ]
            "✏️ EDIT SHOW"
      Nothing ->
        Lucid.span_ [Lucid.class_ "text-gray-400 text-sm"] "No show selected"

-- | Tabbed content section with Episodes and Blog tabs
renderContentTabs :: Maybe Shows.Model -> [Episodes.Model] -> [ShowBlogPosts.Model] -> Lucid.Html ()
renderContentTabs selectedShow recentEpisodes blogPosts = do
  Lucid.div_
    [ xData_
        [i|{
        activeTab: localStorage.getItem('dashboardTab') || 'episodes',
        switchTab(tab) {
          this.activeTab = tab;
          localStorage.setItem('dashboardTab', tab);
        }
      }|],
      Lucid.class_ "w-full"
    ]
    $ do
      -- Tab bar
      Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 mb-0"] $ do
        Lucid.div_ [Lucid.class_ "flex border-b-2 border-gray-800"] $ do
          -- Episodes tab
          Lucid.button_
            [ xOnClick_ "switchTab('episodes')",
              xBindClass_ "activeTab === 'episodes' ? 'bg-gray-800 text-white' : 'bg-white text-gray-800 hover:bg-gray-100'",
              Lucid.class_ "flex-1 px-6 py-3 font-bold text-center transition-colors"
            ]
            "EPISODES"
          -- Blog tab
          Lucid.button_
            [ xOnClick_ "switchTab('blog')",
              xBindClass_ "activeTab === 'blog' ? 'bg-gray-800 text-white' : 'bg-white text-gray-800 hover:bg-gray-100'",
              Lucid.class_ "flex-1 px-6 py-3 font-bold text-center transition-colors"
            ]
            "BLOG"

      -- Episodes content
      Lucid.div_ [xShow_ "activeTab === 'episodes'"] $
        renderRecentEpisodesSection selectedShow recentEpisodes

      -- Blog content
      Lucid.div_ [xShow_ "activeTab === 'blog'"] $
        renderRecentBlogPostsSection selectedShow blogPosts

-- | Recent episodes section (for tab content)
renderRecentEpisodesSection :: Maybe Shows.Model -> [Episodes.Model] -> Lucid.Html ()
renderRecentEpisodesSection selectedShow recentEpisodes =
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-t-0 border-gray-800 p-6 mb-8"] $ do
    Lucid.div_ [Lucid.class_ "flex justify-between items-center mb-4"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold"] "RECENT EPISODES"
      case selectedShow of
        Just showModel -> do
          let uploadUrl = Links.linkURI $ episodesNewGetLink showModel.slug
          Lucid.a_
            [ Lucid.href_ [i|/#{uploadUrl}|],
              hxGet_ [i|/#{uploadUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "bg-blue-600 text-white px-4 py-2 text-sm font-bold hover:bg-blue-700"
            ]
            "🎵 PREPARE SHOW"
        Nothing ->
          Lucid.span_ [Lucid.class_ "bg-gray-400 text-white px-4 py-2 text-sm font-bold opacity-50 cursor-not-allowed"] "🎵 PREPARE SHOW"
    case recentEpisodes of
      [] ->
        Lucid.div_ [Lucid.class_ "text-gray-600 text-center p-8"] $ do
          Lucid.p_ "No episodes uploaded yet."
          Lucid.p_ [Lucid.class_ "text-sm mt-2"] "Use 'PREPARE SHOW' to upload your first episode."
      _ ->
        renderTable
          TableConfig
            { headers =
                [ ColumnHeader "#" AlignLeft,
                  ColumnHeader "TITLE" AlignLeft,
                  ColumnHeader "SCHEDULED" AlignLeft,
                  ColumnHeader "STATUS" AlignLeft,
                  ColumnHeader "ACTIONS" AlignRight
                ],
              wrapperClass = "overflow-x-auto",
              tableClass = "w-full"
            }
          $ mapM_ (maybe (const mempty) renderEpisodeTableRow selectedShow)
          $ take 10 recentEpisodes

-- | Recent blog posts section (for tab content)
renderRecentBlogPostsSection :: Maybe Shows.Model -> [ShowBlogPosts.Model] -> Lucid.Html ()
renderRecentBlogPostsSection selectedShow blogPosts =
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-t-0 border-gray-800 p-6 mb-8"] $ do
    Lucid.div_ [Lucid.class_ "flex justify-between items-center mb-4"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold"] "RECENT BLOG POSTS"
      case selectedShow of
        Just showModel -> do
          let newBlogUrl = Links.linkURI $ showBlogNewGetLink showModel.slug
          Lucid.a_
            [ Lucid.href_ [i|/#{newBlogUrl}|],
              hxGet_ [i|/#{newBlogUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "bg-green-600 text-white px-4 py-2 text-sm font-bold hover:bg-green-700 no-underline"
            ]
            "📝 NEW POST"
        Nothing ->
          let newBlogUrl = Links.linkURI blogNewGetLink
           in Lucid.a_
                [ Lucid.href_ [i|/#{newBlogUrl}|],
                  hxGet_ [i|/#{newBlogUrl}|],
                  hxTarget_ "#main-content",
                  hxPushUrl_ "true",
                  Lucid.class_ "bg-green-600 text-white px-4 py-2 text-sm font-bold hover:bg-green-700 no-underline"
                ]
                "📝 NEW POST"
    case blogPosts of
      [] ->
        Lucid.div_ [Lucid.class_ "text-gray-600 text-center p-8"] $ do
          Lucid.p_ "No blog posts published yet."
          Lucid.p_ [Lucid.class_ "text-sm mt-2"] "Share your thoughts with your audience!"
      _ ->
        renderTable
          TableConfig
            { headers =
                [ ColumnHeader "TITLE" AlignLeft,
                  ColumnHeader "PUBLISHED" AlignLeft,
                  ColumnHeader "STATUS" AlignLeft,
                  ColumnHeader "ACTIONS" AlignRight
                ],
              wrapperClass = "overflow-x-auto",
              tableClass = "w-full"
            }
          $ mapM_ renderBlogPostTableRow
          $ take 10 blogPosts
