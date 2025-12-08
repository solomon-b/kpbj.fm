{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.Slug.Get.Templates.Page
  ( template,
    notFoundTemplate,
    errorTemplate,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Get.Templates.Episode (renderEpisodeCard, renderLatestEpisode)
import API.Dashboard.Shows.Slug.Get.Templates.ShowHeader (renderShowHeader)
import API.Links (dashboardShowsLinks, showBlogLinks)
import API.Shows.Slug.Blog.Get.Templates.PostCard (renderPostCard)
import API.Types (DashboardShowsRoutes (..), ShowBlogRoutes (..))
import Control.Monad (unless, when)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras
import Rel8 (Result)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
showsGetUrl :: Links.URI
showsGetUrl = Links.linkURI $ dashboardShowsLinks.list Nothing Nothing Nothing

--------------------------------------------------------------------------------

-- | Template for show not found
notFoundTemplate :: Slug -> Lucid.Html ()
notFoundTemplate slug = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Show Not Found"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] $ "The show '" <> Lucid.toHtml (display slug) <> "' could not be found."
    Lucid.a_
      [ Lucid.href_ [i|/#{showsGetUrl}|],
        hxGet_ [i|/#{showsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
      ]
      "BROWSE ALL SHOWS"

-- | Template for general error
errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Error"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] $ Lucid.toHtml errorMsg
    Lucid.a_
      [ Lucid.href_ [i|/#{showsGetUrl}|],
        hxGet_ [i|/#{showsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
      ]
      "BROWSE ALL SHOWS"

-- | Main show page template
template :: Shows.Model -> [Episodes.Model] -> [ShowHost.ShowHostWithUser] -> [ShowSchedule.ScheduleTemplate Result] -> [ShowBlogPosts.Model] -> Int -> Lucid.Html ()
template showModel episodes hosts schedules blogPosts currentPage = do
  renderShowHeader showModel hosts schedules

  -- Tabbed Content with Alpine.js
  Lucid.div_
    [ xData_ "{ activeTab: 'episodes' }",
      Lucid.class_ "w-full"
    ]
    $ do
      -- Content Tabs Navigation
      Lucid.div_ [Lucid.class_ "mb-8 w-full"] $ do
        Lucid.div_ [Lucid.class_ "border-b-2 border-gray-800"] $ do
          Lucid.nav_ [Lucid.class_ "flex gap-8"] $ do
            Lucid.button_
              [ xOnClick_ "activeTab = 'episodes'",
                xBindClass_ "activeTab === 'episodes' ? 'py-3 px-4 font-bold uppercase border-b-2 border-gray-800 bg-white -mb-0.5' : 'py-3 px-4 font-bold uppercase text-gray-600 hover:text-gray-800'",
                Lucid.type_ "button"
              ]
              "Episodes"
            Lucid.button_
              [ xOnClick_ "activeTab = 'blog'",
                xBindClass_ "activeTab === 'blog' ? 'py-3 px-4 font-bold uppercase border-b-2 border-gray-800 bg-white -mb-0.5' : 'py-3 px-4 font-bold uppercase text-gray-600 hover:text-gray-800'",
                Lucid.type_ "button"
              ]
              "Blog"

      -- Episodes Tab Content
      Lucid.section_ [Lucid.class_ "w-full", xShow_ "activeTab === 'episodes'"] $ do
        renderEpisodesContent showModel episodes currentPage

      -- Blog Tab Content
      Lucid.section_ [Lucid.class_ "w-full", xShow_ "activeTab === 'blog'"] $ do
        renderBlogContent showModel blogPosts

-- | Number of episodes per page (should match handler)
episodesPerPage :: Int
episodesPerPage = 10

-- Helper function to render episodes content
renderEpisodesContent :: Shows.Model -> [Episodes.Model] -> Int -> Lucid.Html ()
renderEpisodesContent showModel episodes currentPage = do
  if null episodes
    then do
      Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "No Episodes Yet"
        Lucid.p_ [Lucid.class_ "text-gray-600 mb-6"] "This show hasn't published any episodes yet. Check back soon!"
    else do
      -- Featured/Latest Episode with tracks (only on page 1)
      case episodes of
        (latestEpisode : otherEpisodes) -> do
          -- Only show featured episode on first page
          if currentPage == 1
            then do
              renderLatestEpisode showModel latestEpisode []
              unless (null otherEpisodes) $ do
                Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
                  Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Previous Episodes"
                  mapM_ (renderEpisodeCard showModel) otherEpisodes
            else do
              -- On subsequent pages, show all episodes as cards
              Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
                Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-4 uppercase border-b border-gray-800 pb-2"] "Episodes"
                mapM_ (renderEpisodeCard showModel) episodes

          -- Pagination controls
          renderPagination showModel currentPage (length episodes)
        _ -> mempty

-- | Render pagination controls
renderPagination :: Shows.Model -> Int -> Int -> Lucid.Html ()
renderPagination showModel currentPage episodeCount = do
  let hasNextPage = episodeCount >= episodesPerPage
      hasPrevPage = currentPage > 1
      showSlug = Shows.slug showModel
      showId = Shows.id showModel

  when (hasPrevPage || hasNextPage) $ do
    Lucid.div_ [Lucid.class_ "flex justify-center gap-4 mt-6"] $ do
      -- Previous page button
      if hasPrevPage
        then do
          let prevUrl = Links.linkURI $ dashboardShowsLinks.detail showId showSlug (Just (currentPage - 1))
          Lucid.a_
            [ Lucid.href_ [i|/#{prevUrl}|],
              hxGet_ [i|/#{prevUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "bg-gray-800 text-white px-4 py-2 font-bold hover:bg-gray-700"
            ]
            "Previous"
        else
          Lucid.span_
            [Lucid.class_ "bg-gray-300 text-gray-500 px-4 py-2 font-bold cursor-not-allowed"]
            "Previous"

      -- Page indicator
      Lucid.span_ [Lucid.class_ "px-4 py-2 font-bold"] $
        Lucid.toHtml $
          "Page " <> show currentPage

      -- Next page button
      if hasNextPage
        then do
          let nextUrl = Links.linkURI $ dashboardShowsLinks.detail showId showSlug (Just (currentPage + 1))
          Lucid.a_
            [ Lucid.href_ [i|/#{nextUrl}|],
              hxGet_ [i|/#{nextUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "bg-gray-800 text-white px-4 py-2 font-bold hover:bg-gray-700"
            ]
            "Next"
        else
          Lucid.span_
            [Lucid.class_ "bg-gray-300 text-gray-500 px-4 py-2 font-bold cursor-not-allowed"]
            "Next"

-- Helper function to render blog content
renderBlogContent :: Shows.Model -> [ShowBlogPosts.Model] -> Lucid.Html ()
renderBlogContent showModel blogPosts = do
  if null blogPosts
    then do
      Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "No Blog Posts Yet"
        Lucid.p_ [Lucid.class_ "text-gray-600 mb-6"] "This show hasn't published any blog posts yet. Check back soon!"
    else do
      Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6"] $ do
        mapM_ (renderPostCard showModel) blogPosts

      -- View all link
      let blogUrl = Links.linkURI $ showBlogLinks.list (Shows.slug showModel) Nothing Nothing
      Lucid.div_ [Lucid.class_ "mt-8 text-center"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{blogUrl}|],
            hxGet_ [i|/#{blogUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "inline-block bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
          ]
          "View All Blog Posts"
