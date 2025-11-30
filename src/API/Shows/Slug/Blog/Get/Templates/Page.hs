{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Slug.Blog.Get.Templates.Page where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showBlogGetLink)
import API.Shows.Slug.Blog.Get.Templates.PostCard (renderPostCard)
import Control.Monad (forM_, unless, when)
import Data.Int (Int64)
import Data.Maybe (isNothing)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
showBlogGetUrl :: Slug -> Maybe Int64 -> Maybe Text -> Links.URI
showBlogGetUrl slug page tag = Links.linkURI $ showBlogGetLink slug page tag

--------------------------------------------------------------------------------

template :: Shows.Model -> [ShowBlogPosts.Model] -> [ShowBlogTags.Model] -> Maybe Text -> Int64 -> Int64 -> Lucid.Html ()
template showModel posts tags maybeTag currentPage totalPages = do
  Lucid.div_ [Lucid.class_ "max-w-7xl mx-auto px-4 py-12"] $ do
    -- Header
    Lucid.div_ [Lucid.class_ "mb-8"] $ do
      -- Title
      Lucid.h1_ [Lucid.class_ "text-4xl font-bold mb-2"] $ do
        Lucid.toHtml (Shows.title showModel)
        Lucid.span_ [Lucid.class_ "text-gray-600"] " Blog"

      -- Description
      case maybeTag of
        Nothing ->
          Lucid.p_ [Lucid.class_ "text-lg text-gray-600"] "News, updates, and stories from the show"
        Just tag ->
          Lucid.p_ [Lucid.class_ "text-lg text-gray-600"] $ do
            "Posts tagged with "
            Lucid.span_ [Lucid.class_ "font-bold"] $ Lucid.toHtml tag

    -- Tag filter
    unless (null tags) $ do
      Lucid.div_ [Lucid.class_ "mb-8 pb-6 border-b-2 border-gray-800"] $ do
        Lucid.div_ [Lucid.class_ "flex flex-wrap gap-2"] $ do
          -- "All" tag
          Lucid.a_
            [ Lucid.href_ [i|/#{showBlogGetUrl (Shows.slug showModel) Nothing Nothing}|],
              hxGet_ [i|/#{showBlogGetUrl (Shows.slug showModel) Nothing Nothing}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ $
                if isNothing maybeTag
                  then "px-4 py-2 bg-gray-800 text-white border-2 border-gray-800 font-bold"
                  else "px-4 py-2 bg-white text-gray-800 border-2 border-gray-800 hover:bg-gray-100 font-bold"
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
                Lucid.class_ $
                  if maybeTag == Just tagName
                    then "px-4 py-2 bg-gray-800 text-white border-2 border-gray-800 font-bold"
                    else "px-4 py-2 bg-white text-gray-800 border-2 border-gray-800 hover:bg-gray-100 font-bold"
              ]
              $ Lucid.toHtml tagName

    -- Posts grid
    if null posts
      then Lucid.div_ [Lucid.class_ "text-center py-12"] $ do
        Lucid.p_ [Lucid.class_ "text-xl text-gray-600"] "No blog posts yet."
        Lucid.p_ [Lucid.class_ "text-gray-500 mt-2"] "Check back soon for updates!"
      else do
        Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6 mb-8"] $ do
          forM_ posts $ \post -> do
            renderPostCard showModel post

        -- Pagination
        when (totalPages > 1) $ do
          renderPagination showModel maybeTag currentPage totalPages

--------------------------------------------------------------------------------

renderPagination :: Shows.Model -> Maybe Text -> Int64 -> Int64 -> Lucid.Html ()
renderPagination showModel maybeTag currentPage totalPages = do
  Lucid.div_ [Lucid.class_ "flex justify-center items-center gap-2 mt-8"] $ do
    -- Previous button
    when (currentPage > 1) $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogGetUrl (Shows.slug showModel) (Just (currentPage - 1)) maybeTag}|],
          hxGet_ [i|/#{showBlogGetUrl (Shows.slug showModel) (Just (currentPage - 1)) maybeTag}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "px-4 py-2 bg-white text-gray-800 border-2 border-gray-800 hover:bg-gray-100 font-bold"
        ]
        "← Previous"

    -- Page numbers
    forM_ [1 .. totalPages] $ \page -> do
      Lucid.a_
        [ Lucid.href_ [i|/#{showBlogGetUrl (Shows.slug showModel) (Just page) maybeTag}|],
          hxGet_ [i|/#{showBlogGetUrl (Shows.slug showModel) (Just page) maybeTag}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ $
            if page == currentPage
              then "px-4 py-2 bg-gray-800 text-white border-2 border-gray-800 font-bold"
              else "px-4 py-2 bg-white text-gray-800 border-2 border-gray-800 hover:bg-gray-100 font-bold"
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
          Lucid.class_ "px-4 py-2 bg-white text-gray-800 border-2 border-gray-800 hover:bg-gray-100 font-bold"
        ]
        "Next →"

--------------------------------------------------------------------------------

notFoundTemplate :: Slug -> Lucid.Html ()
notFoundTemplate slug = do
  Lucid.div_ [Lucid.class_ "max-w-7xl mx-auto px-4 py-12"] $ do
    Lucid.div_ [Lucid.class_ "text-center"] $ do
      Lucid.h1_ [Lucid.class_ "text-4xl font-bold mb-4"] "Show Not Found"
      Lucid.p_ [Lucid.class_ "text-xl text-gray-600 mb-8"] $ do
        "We couldn't find a show with the slug: "
        Lucid.code_ [Lucid.class_ "bg-gray-100 px-2 py-1"] $ Lucid.toHtml (display slug)

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "max-w-7xl mx-auto px-4 py-12"] $ do
    Lucid.div_ [Lucid.class_ "bg-red-50 border-2 border-red-600 p-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-2 text-red-600"] "Error"
      Lucid.p_ [Lucid.class_ "text-red-700"] $ Lucid.toHtml errorMsg
