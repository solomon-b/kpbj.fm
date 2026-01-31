{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Get.Templates.Page
  ( template,
    notFoundTemplate,
    errorTemplate,
  )
where

--------------------------------------------------------------------------------

import API.Links (showsLinks)
import API.Shows.Slug.Get.Templates.ShowHeader (renderShowHeader)
import API.Types
import Component.Card.Episode (renderEpisodeCard)
import Control.Monad (when)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_, desktop, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.HTMX
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
  Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.cardBorder, Tokens.p8, "text-center"]] $ do
    Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] "Show Not Found"
    Lucid.p_ [class_ $ base [Tokens.mb4, Tokens.fgMuted]] $ "The show '" <> Lucid.toHtml (display slug) <> "' could not be found."
    Lucid.a_
      [ Lucid.href_ [i|/#{showsGetUrl}|],
        hxGet_ [i|/#{showsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        class_ $ base [Tokens.bgInverse, Tokens.fgInverse, Tokens.px6, "py-3", Tokens.fontBold, "hover:opacity-80"]
      ]
      "BROWSE ALL SHOWS"

-- | Template for general error
errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.cardBorder, Tokens.p8, "text-center"]] $ do
    Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] "Error"
    Lucid.p_ [class_ $ base [Tokens.mb4, Tokens.fgMuted]] $ Lucid.toHtml errorMsg
    Lucid.a_
      [ Lucid.href_ [i|/#{showsGetUrl}|],
        hxGet_ [i|/#{showsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        class_ $ base [Tokens.bgInverse, Tokens.fgInverse, Tokens.px6, "py-3", Tokens.fontBold, "hover:opacity-80"]
      ]
      "BROWSE ALL SHOWS"

-- | Main show page template
template :: StorageBackend -> Shows.Model -> [Episodes.Model] -> [ShowHost.ShowHostWithUser] -> [ShowSchedule.ScheduleTemplate Result] -> [ShowBlogPosts.Model] -> [ShowTags.Model] -> Int -> Lucid.Html ()
template backend showModel episodes hosts schedules _blogPosts tags currentPage = do
  renderShowHeader backend showModel hosts schedules tags

  -- Episodes content
  Lucid.section_ [Lucid.class_ Tokens.fullWidth] $ do
    renderEpisodesContent backend showModel episodes currentPage

-- | Number of episodes per page (should match handler)
episodesPerPage :: Int
episodesPerPage = 10

-- Helper function to render episodes content
renderEpisodesContent :: StorageBackend -> Shows.Model -> [Episodes.Model] -> Int -> Lucid.Html ()
renderEpisodesContent backend showModel episodes currentPage = do
  if null episodes
    then do
      Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.cardBorder, Tokens.p8, "text-center"]] $ do
        Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold, Tokens.mb4]] "No Episodes Yet"
        Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.mb6]] "This show hasn't published any episodes yet. Check back soon!"
    else do
      -- Grid layout: 1 column on mobile, 2 on tablet, 3 on desktop
      Lucid.div_ [class_ $ do { base ["grid", "grid-cols-1", Tokens.gap6]; tablet ["grid-cols-2"]; desktop ["grid-cols-3"] }] $ do
        mapM_ (renderEpisodeCard backend showModel) episodes
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
              class_ $ base [Tokens.bgInverse, Tokens.fgInverse, Tokens.px4, Tokens.py2, Tokens.fontBold, "hover:opacity-80"]
            ]
            "Previous"
        else
          Lucid.span_
            [class_ $ base [Tokens.bgAlt, Tokens.fgMuted, Tokens.px4, Tokens.py2, Tokens.fontBold, "cursor-not-allowed"]]
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
              class_ $ base [Tokens.bgInverse, Tokens.fgInverse, Tokens.px4, Tokens.py2, Tokens.fontBold, "hover:opacity-80"]
            ]
            "Next"
        else
          Lucid.span_
            [class_ $ base [Tokens.bgAlt, Tokens.fgMuted, Tokens.px4, Tokens.py2, Tokens.fontBold, "cursor-not-allowed"]]
            "Next"
