{-# LANGUAGE QuasiQuotes #-}

module API.Blog.Get.Templates.Pagination
  ( renderPagination,
  )
where

import API.Links (blogLinks)
import API.Types
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Lucid.Responsive (cls)
import Servant.Links qualified as Links

-- | Render pagination controls
renderPagination :: Int64 -> Bool -> Lucid.Html ()
renderPagination currentPage hasMore = do
  Lucid.div_ [Lucid.class_ "flex justify-center mt-8"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center space-x-2"] $ do
      -- Previous button
      if currentPage > 1
        then
          Lucid.a_
            [ Lucid.href_ [i|/#{blogGetPageUrl (currentPage - 1)}|],
              hxGet_ [i|/#{blogGetPageUrl (currentPage - 1)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ paginationLink
            ]
            "‹ Previous"
        else Lucid.span_ [Lucid.class_ paginationDisabled] "‹ Previous"

      -- Current page
      Lucid.span_ [Lucid.class_ paginationActive] $
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
              Lucid.class_ paginationLink
            ]
            "Next ›"
        else Lucid.span_ [Lucid.class_ paginationDisabled] "Next ›"
  where
    blogGetPageUrl :: Int64 -> Links.URI
    blogGetPageUrl page = Links.linkURI $ blogLinks.list (Just page) Nothing

    paginationLink = cls ["px-3 py-1", Tokens.textGray800, "hover:bg-gray-200"]
    paginationDisabled = "px-3 py-1 text-gray-400"
    paginationActive = cls ["px-3 py-1", Tokens.bgGray800, Tokens.textWhite, Tokens.fontBold]
