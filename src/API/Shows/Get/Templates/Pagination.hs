{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Get.Templates.Pagination
  ( renderPagination,
  )
where

import {-# SOURCE #-} API (showsGetLink)
import Data.String.Interpolate (i)
import Data.Text.Display (display)
import Domain.Types.PageNumber (PageNumber)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

-- | Render pagination controls
renderPagination :: PageNumber -> Bool -> Lucid.Html ()
renderPagination currentPage hasMore = do
  Lucid.div_ [Lucid.class_ "flex justify-center mt-8"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center space-x-2"] $ do
      -- Previous button
      if currentPage > 1
        then
          Lucid.a_
            [ Lucid.href_ [i|/#{showsGetPageUrl (currentPage - 1)}|],
              hxGet_ [i|/#{showsGetPageUrl (currentPage - 1)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "px-3 py-1 text-gray-800 hover:bg-gray-200"
            ]
            "‹ Previous"
        else Lucid.span_ [Lucid.class_ "px-3 py-1 text-gray-400"] "‹ Previous"

      -- Current page
      Lucid.span_ [Lucid.class_ "px-3 py-1 bg-gray-800 text-white font-bold"] $
        Lucid.toHtml $
          display currentPage

      -- Next button
      if hasMore
        then
          Lucid.a_
            [ Lucid.href_ [i|/#{showsGetPageUrl (currentPage + 1)}|],
              hxGet_ [i|/#{showsGetPageUrl (currentPage + 1)}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ "px-3 py-1 text-gray-800 hover:bg-gray-200"
            ]
            "Next ›"
        else Lucid.span_ [Lucid.class_ "px-3 py-1 text-gray-400"] "Next ›"
  where
    showsGetPageUrl :: PageNumber -> Links.URI
    showsGetPageUrl page = Links.linkURI $ showsGetLink (Just page) Nothing Nothing Nothing
