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
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    PaginationConfig (..),
    renderIndexTable,
  )
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
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
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, "overflow-hidden"]] $
    case (blogPosts, selectedShow) of
      ([], _) ->
        renderEmptyState
      (_, Nothing) ->
        mempty
      (_, Just showModel) ->
        renderIndexTable
          IndexTableConfig
            { itcBodyId = "blog-posts-table-body",
              itcHeaders =
                [ ColumnHeader "TITLE" AlignLeft,
                  ColumnHeader "PUBLISHED" AlignLeft,
                  ColumnHeader "STATUS" AlignLeft,
                  ColumnHeader "ACTIONS" AlignRight
                ],
              itcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl showModel}|] else Nothing,
              itcPaginationConfig =
                Just
                  PaginationConfig
                    { pcPrevPageUrl = if currentPage > 1 then Just [i|/#{prevPageUrl showModel}|] else Nothing,
                      pcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl showModel}|] else Nothing,
                      pcCurrentPage = currentPage
                    }
            }
          (mapM_ (renderBlogPostTableRow showModel) blogPosts)
  where
    nextPageUrl :: Shows.Model -> Links.URI
    nextPageUrl showModel = Links.linkURI $ dashboardBlogsLinks.list showModel.slug (Just (currentPage + 1))
    prevPageUrl :: Shows.Model -> Links.URI
    prevPageUrl showModel = Links.linkURI $ dashboardBlogsLinks.list showModel.slug (Just (currentPage - 1))

-- | Empty state when no blog posts exist
renderEmptyState :: Lucid.Html ()
renderEmptyState =
  Lucid.div_ [class_ $ base [Tokens.textGray600, "text-center", Tokens.p8]] $ do
    Lucid.p_ "No blog posts yet."
    Lucid.p_ [class_ $ base [Tokens.textSm, "mt-2"]] "Share your thoughts with your audience!"
