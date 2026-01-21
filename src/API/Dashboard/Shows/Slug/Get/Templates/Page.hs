{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.Slug.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Get.Templates.Episode (renderEpisodeCard, renderLatestEpisode)
import API.Dashboard.Shows.Slug.Get.Templates.ShowHeader (renderShowHeader)
import API.Links (dashboardShowsLinks, showBlogLinks)
import API.Types (DashboardShowsRoutes (..), ShowBlogRoutes (..))
import Component.Card.BlogPost (renderShowBlogPostCard)
import Control.Monad (unless, when)
import Data.String.Interpolate (i)
import Design (base, class_, class_', desktop, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras
import Rel8 (Result)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Main show page template
template :: StorageBackend -> Shows.Model -> [Episodes.Model] -> [ShowHost.ShowHostWithUser] -> [ShowSchedule.ScheduleTemplate Result] -> [ShowBlogPosts.Model] -> [ShowTags.Model] -> Int -> Lucid.Html ()
template backend showModel episodes hosts schedules blogPosts tags currentPage = do
  renderShowHeader backend showModel hosts schedules tags

  -- Tabbed Content with Alpine.js
  Lucid.div_
    [ xData_ "{ activeTab: 'episodes' }",
      Lucid.class_ Tokens.fullWidth
    ]
    $ do
      -- Content Tabs Navigation
      Lucid.div_ [class_ $ base [Tokens.mb8, Tokens.fullWidth]] $ do
        Lucid.div_ [class_ $ base ["border-b-2", Tokens.borderGray800]] $ do
          Lucid.nav_ [class_ $ base ["flex", Tokens.gap8]] $ do
            Lucid.button_
              [ xOnClick_ "activeTab = 'episodes'",
                xBindClass_ [i|activeTab === 'episodes' ? '#{class_' $ base ["py-3", Tokens.px4, Tokens.fontBold, "uppercase", "border-b-2", Tokens.borderGray800, Tokens.bgWhite, "-mb-0.5"]}' : '#{class_' $ base ["py-3", Tokens.px4, Tokens.fontBold, "uppercase", Tokens.textGray600, "hover:text-gray-800"]}'|],
                Lucid.type_ "button"
              ]
              "Episodes"
            Lucid.button_
              [ xOnClick_ "activeTab = 'blog'",
                xBindClass_ [i|activeTab === 'blog' ? '#{class_' $ base ["py-3", Tokens.px4, Tokens.fontBold, "uppercase", "border-b-2", Tokens.borderGray800, Tokens.bgWhite, "-mb-0.5"]}' : '#{class_' $ base ["py-3", Tokens.px4, Tokens.fontBold, "uppercase", Tokens.textGray600, "hover:text-gray-800"]}'|],
                Lucid.type_ "button"
              ]
              "Blog"

      -- Episodes Tab Content
      Lucid.section_ [Lucid.class_ Tokens.fullWidth, xShow_ "activeTab === 'episodes'"] $ do
        renderEpisodesContent backend showModel episodes currentPage

      -- Blog Tab Content
      Lucid.section_ [Lucid.class_ Tokens.fullWidth, xShow_ "activeTab === 'blog'"] $ do
        renderBlogContent backend showModel blogPosts

-- | Number of episodes per page (should match handler)
episodesPerPage :: Int
episodesPerPage = 10

-- Helper function to render episodes content
renderEpisodesContent :: StorageBackend -> Shows.Model -> [Episodes.Model] -> Int -> Lucid.Html ()
renderEpisodesContent backend showModel episodes currentPage = do
  if null episodes
    then do
      Lucid.div_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, "text-center"]] $ do
        Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] "No Episodes Yet"
        Lucid.p_ [class_ $ base [Tokens.textGray600, Tokens.mb6]] "This show hasn't published any episodes yet. Check back soon!"
    else do
      -- Featured/Latest Episode with tracks (only on page 1)
      case episodes of
        (latestEpisode : otherEpisodes) -> do
          -- Only show featured episode on first page
          if currentPage == 1
            then do
              renderLatestEpisode backend showModel latestEpisode []
              unless (null otherEpisodes) $ do
                Lucid.div_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6]] $ do
                  Lucid.h3_ [class_ $ base [Tokens.textLg, Tokens.fontBold, Tokens.mb4, "uppercase", "border-b", Tokens.borderGray800, Tokens.pb2]] "Previous Episodes"
                  mapM_ (renderEpisodeCard backend showModel) otherEpisodes
            else do
              -- On subsequent pages, show all episodes as cards
              Lucid.div_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6]] $ do
                Lucid.h3_ [class_ $ base [Tokens.textLg, Tokens.fontBold, Tokens.mb4, "uppercase", "border-b", Tokens.borderGray800, Tokens.pb2]] "Episodes"
                mapM_ (renderEpisodeCard backend showModel) episodes

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
    Lucid.div_ [class_ $ base ["flex", "justify-center", Tokens.gap4, "mt-6"]] $ do
      -- Previous page button
      if hasPrevPage
        then do
          let prevUrl = Links.linkURI $ dashboardShowsLinks.detail showId showSlug (Just (currentPage - 1))
          Lucid.a_
            [ Lucid.href_ [i|/#{prevUrl}|],
              hxGet_ [i|/#{prevUrl}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.px4, Tokens.py2, Tokens.fontBold, "hover:bg-gray-700"]
            ]
            "Previous"
        else
          Lucid.span_
            [class_ $ base ["bg-gray-300 dark:bg-gray-600", "text-gray-500 dark:text-gray-400", Tokens.px4, Tokens.py2, Tokens.fontBold, "cursor-not-allowed"]]
            "Previous"

      -- Page indicator
      Lucid.span_ [class_ $ base [Tokens.px4, Tokens.py2, Tokens.fontBold]] $
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
              class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.px4, Tokens.py2, Tokens.fontBold, "hover:bg-gray-700"]
            ]
            "Next"
        else
          Lucid.span_
            [class_ $ base ["bg-gray-300 dark:bg-gray-600", "text-gray-500 dark:text-gray-400", Tokens.px4, Tokens.py2, Tokens.fontBold, "cursor-not-allowed"]]
            "Next"

-- Helper function to render blog content
renderBlogContent :: StorageBackend -> Shows.Model -> [ShowBlogPosts.Model] -> Lucid.Html ()
renderBlogContent backend showModel blogPosts = do
  if null blogPosts
    then do
      Lucid.div_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, "text-center"]] $ do
        Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] "No Blog Posts Yet"
        Lucid.p_ [class_ $ base [Tokens.textGray600, Tokens.mb6]] "This show hasn't published any blog posts yet. Check back soon!"
    else do
      Lucid.div_ [class_ $ do { base ["grid", "grid-cols-1", Tokens.gap6]; tablet ["grid-cols-2"]; desktop ["grid-cols-3"] }] $ do
        mapM_ (renderShowBlogPostCard backend showModel) blogPosts

      -- View all link
      let blogUrl = Links.linkURI $ showBlogLinks.list (Shows.slug showModel) Nothing Nothing
      Lucid.div_ [class_ $ base ["mt-8", "text-center"]] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{blogUrl}|],
            hxGet_ [i|/#{blogUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["inline-block", Tokens.bgGray800, Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700"]
          ]
          "View All Blog Posts"
