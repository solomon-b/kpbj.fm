{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Blog.Get.Templates.Page where

--------------------------------------------------------------------------------

import API.Links (apiLinks)
import API.Types
import Component.Card.BlogPost (renderShowBlogPostCard)
import Control.Monad (forM_, unless, when)
import Data.Int (Int64)
import Data.Maybe (isNothing)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_, desktop, tablet)
import Design qualified
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Base (Attributes)
import Lucid.Extras
import Servant.Links qualified as Links

--------------------------------------------------------------------------------
-- Helpers

-- | Toggle button style for active/inactive states
toggleStyle :: Bool -> Attributes
toggleStyle isActive = class_ $ do
  base [Tokens.px4, Tokens.py2, Tokens.cardBorder, Tokens.fontBold]
  Design.when isActive $ base [Tokens.bgGray800, Tokens.textWhite]
  Design.when (not isActive) $ base [Tokens.bgWhite, Tokens.textGray800, "hover:bg-gray-100"]

--------------------------------------------------------------------------------

-- URL helpers
showBlogGetUrl :: Slug -> Maybe Int64 -> Maybe Text -> Links.URI
showBlogGetUrl slug page tag = Links.linkURI $ apiLinks.shows.blog.list slug page tag

--------------------------------------------------------------------------------

template :: StorageBackend -> Shows.Model -> [ShowBlogPosts.Model] -> [ShowBlogTags.Model] -> Maybe Text -> Int64 -> Int64 -> Lucid.Html ()
template backend showModel posts tags maybeTag currentPage totalPages = do
  Lucid.div_ [class_ $ base ["max-w-7xl", "mx-auto", Tokens.px4, "py-12"]] $ do
    -- Header
    Lucid.div_ [Lucid.class_ Tokens.mb8] $ do
      -- Title
      Lucid.h1_ [class_ $ base ["text-4xl", Tokens.fontBold, Tokens.mb2]] $ do
        Lucid.toHtml (Shows.title showModel)
        Lucid.span_ [Lucid.class_ Tokens.textGray600] " Blog"

      -- Description
      case maybeTag of
        Nothing ->
          Lucid.p_ [class_ $ base [Tokens.textLg, Tokens.textGray600]] "News, updates, and stories from the show"
        Just tag ->
          Lucid.p_ [class_ $ base [Tokens.textLg, Tokens.textGray600]] $ do
            "Posts tagged with "
            Lucid.span_ [Lucid.class_ Tokens.fontBold] $ Lucid.toHtml tag

    -- Tag filter
    unless (null tags) $ do
      Lucid.div_ [class_ $ base [Tokens.mb8, Tokens.pb2, "pb-6", "border-b-2", "border-gray-800"]] $ do
        Lucid.div_ [class_ $ base ["flex", "flex-wrap", Tokens.gap2]] $ do
          -- "All" tag
          Lucid.a_
            [ Lucid.href_ [i|/#{showBlogGetUrl (Shows.slug showModel) Nothing Nothing}|],
              hxGet_ [i|/#{showBlogGetUrl (Shows.slug showModel) Nothing Nothing}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              toggleStyle (isNothing maybeTag)
            ]
            "All"

          -- Individual tags
          forM_ tags $ \tag -> do
            let tagName = ShowBlogTags.sbtmName tag
            Lucid.a_
              [ Lucid.href_ [i|/#{showBlogGetUrl (Shows.slug showModel) Nothing (Just tagName)}|],
                hxGet_ [i|/#{showBlogGetUrl (Shows.slug showModel) Nothing (Just tagName)}|],
                hxTarget_ "#main-content",
                hxPushUrl_ "true",
                toggleStyle (maybeTag == Just tagName)
              ]
              $ Lucid.toHtml tagName

    -- Posts grid
    if null posts
      then Lucid.div_ [class_ $ base ["text-center", "py-12"]] $ do
        Lucid.p_ [class_ $ base [Tokens.textXl, Tokens.textGray600]] "No blog posts yet."
        Lucid.p_ [class_ $ base ["text-gray-500", "mt-2"]] "Check back soon for updates!"
      else do
        Lucid.div_ [class_ $ do { base ["grid", "grid-cols-1", Tokens.gap6, Tokens.mb8]; tablet ["grid-cols-2"]; desktop ["grid-cols-3"] }] $ do
          forM_ posts $ \post -> do
            renderShowBlogPostCard backend showModel post

        -- Pagination
        when (totalPages > 1) $ do
          renderPagination showModel maybeTag currentPage totalPages

--------------------------------------------------------------------------------

renderPagination :: Shows.Model -> Maybe Text -> Int64 -> Int64 -> Lucid.Html ()
renderPagination showModel maybeTag currentPage totalPages = do
  Lucid.div_ [class_ $ base ["flex", "justify-center", "items-center", Tokens.gap2, "mt-8"]] $ do
    -- Previous button
    when (currentPage > 1) $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogGetUrl (Shows.slug showModel) (Just (currentPage - 1)) maybeTag}|],
          hxGet_ [i|/#{showBlogGetUrl (Shows.slug showModel) (Just (currentPage - 1)) maybeTag}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base [Tokens.px4, Tokens.py2, Tokens.bgWhite, Tokens.textGray800, Tokens.cardBorder, "hover:bg-gray-100", Tokens.fontBold]
        ]
        "← Previous"

    -- Page numbers
    forM_ [1 .. totalPages] $ \page -> do
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogGetUrl (Shows.slug showModel) (Just page) maybeTag}|],
          hxGet_ [i|/#{showBlogGetUrl (Shows.slug showModel) (Just page) maybeTag}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          toggleStyle (page == currentPage)
        ]
        $ Lucid.toHtml
        $ show page

    -- Next button
    when (currentPage < totalPages) $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogGetUrl (Shows.slug showModel) (Just (currentPage + 1)) maybeTag}|],
          hxGet_ [i|/#{showBlogGetUrl (Shows.slug showModel) (Just (currentPage + 1)) maybeTag}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base [Tokens.px4, Tokens.py2, Tokens.bgWhite, Tokens.textGray800, Tokens.cardBorder, "hover:bg-gray-100", Tokens.fontBold]
        ]
        "Next →"

--------------------------------------------------------------------------------

notFoundTemplate :: Slug -> Lucid.Html ()
notFoundTemplate slug = do
  Lucid.div_ [class_ $ base ["max-w-7xl", "mx-auto", Tokens.px4, "py-12"]] $ do
    Lucid.div_ [Lucid.class_ "text-center"] $ do
      Lucid.h1_ [class_ $ base ["text-4xl", Tokens.fontBold, Tokens.mb4]] "Show Not Found"
      Lucid.p_ [class_ $ base [Tokens.textXl, Tokens.textGray600, Tokens.mb8]] $ do
        "We couldn't find a show with the slug: "
        Lucid.code_ [class_ $ base [Tokens.bgGray100, "px-2", "py-1"]] $ Lucid.toHtml (display slug)

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [class_ $ base ["max-w-7xl", "mx-auto", Tokens.px4, "py-12"]] $ do
    Lucid.div_ [class_ $ base [Tokens.errorBg, Tokens.border2, Tokens.errorBorder, Tokens.p6]] $ do
      Lucid.h2_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2, Tokens.errorText]] "Error"
      Lucid.p_ [Lucid.class_ "text-red-700"] $ Lucid.toHtml errorMsg
