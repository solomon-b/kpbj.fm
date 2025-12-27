{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Blogs.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Blogs.Get.Templates.BlogPostRow (renderBlogPostTableRow)
import API.Links (dashboardBlogsLinks)
import API.Types
import Component.InfiniteScroll (renderEndOfContent, renderLoadingIndicator, renderSentinel)
import Component.Table (ColumnAlign (..), ColumnHeader (..), TableConfig (..), renderTableWithBodyId)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Blog dashboard template (stats are now in the top bar)
template ::
  Maybe Shows.Model ->
  [ShowBlogPosts.Model] ->
  Int64 ->
  Bool ->
  Lucid.Html ()
template = renderBlogSection

-- | Blog posts table section
renderBlogSection :: Maybe Shows.Model -> [ShowBlogPosts.Model] -> Int64 -> Bool -> Lucid.Html ()
renderBlogSection selectedShow blogPosts currentPage hasMore = do
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, "overflow-hidden"]] $ do
    case blogPosts of
      [] ->
        Lucid.div_ [class_ $ base [Tokens.textGray600, "text-center", Tokens.p8]] $ do
          Lucid.p_ "No blog posts yet."
          Lucid.p_ [class_ $ base [Tokens.textSm, "mt-2"]] "Share your thoughts with your audience!"
      _ ->
        case selectedShow of
          Nothing -> mempty
          Just showModel ->
            renderTableWithBodyId
              "blog-posts-table-body"
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
              $ do
                mapM_ (renderBlogPostTableRow showModel) blogPosts
                -- Sentinel row for infinite scroll
                if hasMore
                  then
                    Lucid.tr_ [Lucid.id_ "load-more-sentinel-row"] $
                      Lucid.td_ [Lucid.colspan_ "4"] $
                        renderSentinel [i|/#{nextPageUrl showModel}|] "#blog-posts-table-body"
                  else
                    Lucid.tr_ [] $
                      Lucid.td_ [Lucid.colspan_ "4"] $
                        renderEndOfContent

  -- Loading indicator (hidden by default, shown during HTMX requests)
  renderLoadingIndicator

  -- Fallback pagination for browsers without JavaScript
  Lucid.noscript_ $
    case selectedShow of
      Nothing -> mempty
      Just showModel -> renderPagination showModel currentPage hasMore
  where
    nextPageUrl :: Shows.Model -> Links.URI
    nextPageUrl showModel = Links.linkURI $ dashboardBlogsLinks.list showModel.slug (Just (currentPage + 1))

-- | Pagination controls for noscript fallback
renderPagination :: Shows.Model -> Int64 -> Bool -> Lucid.Html ()
renderPagination showModel currentPage hasMore = do
  Lucid.div_ [class_ $ base ["flex", "justify-between", "items-center", "mt-4"]] $ do
    -- Previous button
    if currentPage > 1
      then
        Lucid.a_
          [ Lucid.href_ [i|/#{prevPageUrl}|],
            hxGet_ [i|/#{prevPageUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700"]
          ]
          "<- PREVIOUS"
      else
        Lucid.div_ [] mempty

    -- Page indicator
    Lucid.span_ [class_ $ base [Tokens.textGray600, Tokens.fontBold]] $
      Lucid.toHtml $
        "Page " <> show currentPage

    -- Next button
    if hasMore
      then
        Lucid.a_
          [ Lucid.href_ [i|/#{nextPageUrl}|],
            hxGet_ [i|/#{nextPageUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700"]
          ]
          "NEXT ->"
      else
        Lucid.div_ [] mempty
  where
    prevPageUrl = Links.linkURI $ dashboardBlogsLinks.list showModel.slug (Just (currentPage - 1))
    nextPageUrl = Links.linkURI $ dashboardBlogsLinks.list showModel.slug (Just (currentPage + 1))
