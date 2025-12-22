{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Get.Templates.Page
  ( template,
    notFoundTemplate,
    errorTemplate,
  )
where

--------------------------------------------------------------------------------

import API.Links (showBlogLinks, showsLinks)
import API.Shows.Slug.Get.Templates.ShowHeader (renderShowHeader)
import API.Types
import Component.Card.BlogPost (renderShowBlogPostCard)
import Component.Card.Episode (renderEpisodeCard)
import Control.Monad (when)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_, desktop, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
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

-- URL helpers
showsGetUrl :: Links.URI
showsGetUrl = Links.linkURI $ showsLinks.list Nothing Nothing Nothing Nothing Nothing

--------------------------------------------------------------------------------

-- | Template for show not found
notFoundTemplate :: Slug -> Lucid.Html ()
notFoundTemplate slug = do
  Lucid.div_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, "text-center"]] $ do
    Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] "Show Not Found"
    Lucid.p_ [class_ $ base [Tokens.mb4, Tokens.textGray600]] $ "The show '" <> Lucid.toHtml (display slug) <> "' could not be found."
    Lucid.a_
      [ Lucid.href_ [i|/#{showsGetUrl}|],
        hxGet_ [i|/#{showsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        class_ $ base ["bg-blue-600", Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-blue-700"]
      ]
      "BROWSE ALL SHOWS"

-- | Template for general error
errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, "text-center"]] $ do
    Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] "Error"
    Lucid.p_ [class_ $ base [Tokens.mb4, Tokens.textGray600]] $ Lucid.toHtml errorMsg
    Lucid.a_
      [ Lucid.href_ [i|/#{showsGetUrl}|],
        hxGet_ [i|/#{showsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        class_ $ base ["bg-blue-600", Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-blue-700"]
      ]
      "BROWSE ALL SHOWS"

-- | Main show page template
template :: Shows.Model -> [Episodes.Model] -> [ShowHost.ShowHostWithUser] -> [ShowSchedule.ScheduleTemplate Result] -> [ShowBlogPosts.Model] -> [ShowTags.Model] -> Int -> Bool -> Lucid.Html ()
template showModel episodes hosts schedules _blogPosts tags currentPage canViewDrafts = do
  renderShowHeader showModel hosts schedules tags

  -- Episodes content
  Lucid.section_ [Lucid.class_ Tokens.fullWidth] $ do
    renderEpisodesContent showModel episodes currentPage canViewDrafts

-- | Number of episodes per page (should match handler)
episodesPerPage :: Int
episodesPerPage = 10

-- Helper function to render episodes content
renderEpisodesContent :: Shows.Model -> [Episodes.Model] -> Int -> Bool -> Lucid.Html ()
renderEpisodesContent showModel episodes currentPage canViewDrafts = do
  if null episodes
    then do
      Lucid.div_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, "text-center"]] $ do
        Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] "No Episodes Yet"
        Lucid.p_ [class_ $ base [Tokens.textGray600, Tokens.mb6]] "This show hasn't published any episodes yet. Check back soon!"
    else do
      -- Grid layout: 1 column on mobile, 2 on tablet, 3 on desktop
      Lucid.div_ [class_ $ do { base ["grid", "grid-cols-1", Tokens.gap6]; tablet ["grid-cols-2"]; desktop ["grid-cols-3"] }] $ do
        mapM_ (renderEpisodeCard showModel canViewDrafts) episodes
      renderPagination showModel currentPage (length episodes)

-- | Render pagination controls
renderPagination :: Shows.Model -> Int -> Int -> Lucid.Html ()
renderPagination showModel currentPage episodeCount = do
  let hasNextPage = episodeCount >= episodesPerPage
      hasPrevPage = currentPage > 1
      showSlug = Shows.slug showModel

  when (hasPrevPage || hasNextPage) $ do
    Lucid.div_ [class_ $ base ["flex", "justify-center", Tokens.gap4, "mt-6"]] $ do
      -- Previous page button
      if hasPrevPage
        then do
          let prevUrl = Links.linkURI $ showsLinks.detail showSlug (Just (currentPage - 1))
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
            [class_ $ base ["bg-gray-300", "text-gray-500", Tokens.px4, Tokens.py2, Tokens.fontBold, "cursor-not-allowed"]]
            "Previous"

      -- Page indicator
      Lucid.span_ [class_ $ base [Tokens.px4, Tokens.py2, Tokens.fontBold]] $
        Lucid.toHtml $
          "Page " <> show currentPage

      -- Next page button
      if hasNextPage
        then do
          let nextUrl = Links.linkURI $ showsLinks.detail showSlug (Just (currentPage + 1))
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
            [class_ $ base ["bg-gray-300", "text-gray-500", Tokens.px4, Tokens.py2, Tokens.fontBold, "cursor-not-allowed"]]
            "Next"

-- Helper function to render blog content (kept for future show blogs feature)
_renderBlogContent :: Shows.Model -> [ShowBlogPosts.Model] -> Lucid.Html ()
_renderBlogContent showModel blogPosts = do
  if null blogPosts
    then do
      Lucid.div_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, "text-center"]] $ do
        Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] "No Blog Posts Yet"
        Lucid.p_ [class_ $ base [Tokens.textGray600, Tokens.mb6]] "This show hasn't published any blog posts yet. Check back soon!"
    else do
      Lucid.div_ [class_ $ do { base ["grid", "grid-cols-1", Tokens.gap6]; tablet ["grid-cols-2"]; desktop ["grid-cols-3"] }] $ do
        mapM_ (renderShowBlogPostCard showModel) blogPosts

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
