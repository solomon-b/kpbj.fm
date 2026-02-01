{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Get.Templates.Pagination
  ( renderPagination,
  )
where

import API.Links (blogLinks)
import API.Types
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

-- | Render pagination controls (used as noscript fallback for infinite scroll)
renderPagination :: Int64 -> Bool -> Maybe Text -> Lucid.Html ()
renderPagination currentPage hasMore maybeTag = do
  Lucid.div_ [Lucid.id_ "pagination-controls", class_ $ base ["flex", "justify-center", "mt-8"]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "space-x-2"]] $ do
      -- Previous button
      if currentPage > 1
        then
          Lucid.a_
            [ Lucid.href_ [i|/#{blogGetPageUrl (currentPage - 1)}|],
              hxGet_ [i|/#{blogGetPageUrl (currentPage - 1)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              paginationLink
            ]
            "‹ Previous"
        else Lucid.span_ [paginationDisabled] "‹ Previous"

      -- Current page
      Lucid.span_ [paginationActive] $
        Lucid.toHtml $
          show currentPage

      -- Next button
      if hasMore
        then
          Lucid.a_
            [ Lucid.href_ [i|/#{blogGetPageUrl (currentPage + 1)}|],
              hxGet_ [i|/#{blogGetPageUrl (currentPage + 1)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              paginationLink
            ]
            "Next ›"
        else Lucid.span_ [paginationDisabled] "Next ›"
  where
    blogGetPageUrl :: Int64 -> Links.URI
    blogGetPageUrl page = Links.linkURI $ blogLinks.list (Just page) maybeTag

    paginationLink = class_ $ base ["px-3 py-1", Tokens.fgPrimary, Tokens.hoverBg]
    paginationDisabled = class_ $ base ["px-3 py-1", Tokens.fgMuted]
    paginationActive = class_ $ base ["px-3 py-1", Tokens.bgInverse, Tokens.fgInverse, Tokens.fontBold]
