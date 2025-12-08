{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationBlog.Get.Templates.Page where

--------------------------------------------------------------------------------

import API.Links (blogLinks, dashboardStationBlogLinks)
import API.Types (BlogRoutes (..), DashboardStationBlogRoutes (..))
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Links

-- | Station blog posts list template
template ::
  [BlogPosts.Model] ->
  Int64 ->
  Bool ->
  Lucid.Html ()
template posts currentPage hasMore = do
  -- Blog posts table or empty state
  if null posts
    then renderEmptyState
    else do
      Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 overflow-hidden mb-8 w-full"] $
        Lucid.table_ [Lucid.class_ "w-full"] $ do
          Lucid.thead_ [Lucid.class_ "bg-gray-800 text-white"] $
            Lucid.tr_ $ do
              Lucid.th_ [Lucid.class_ "p-4 text-left"] "Title"
              Lucid.th_ [Lucid.class_ "p-4 text-left"] "Status"
              Lucid.th_ [Lucid.class_ "p-4 text-left"] "Created"
              Lucid.th_ [Lucid.class_ "p-4 text-left"] "Published"
              Lucid.th_ [Lucid.class_ "p-4 text-center w-24"] ""
          Lucid.tbody_ $
            mapM_ renderPostRow posts

      renderPagination currentPage hasMore

renderPostRow :: BlogPosts.Model -> Lucid.Html ()
renderPostRow post =
  let postId = post.bpmId
      postSlug = post.bpmSlug
      title = post.bpmTitle
      status = post.bpmStatus
      createdAt = post.bpmCreatedAt
      publishedAt = post.bpmPublishedAt
      detailUrl = Links.linkURI $ dashboardStationBlogLinks.detail postId postSlug
      editUrl = Links.linkURI $ dashboardStationBlogLinks.editGet postId postSlug
      viewUrl = Links.linkURI $ blogLinks.postWithSlug postId postSlug
      deleteUrl = Links.linkURI $ dashboardStationBlogLinks.delete postId postSlug
      cellLinkAttrs =
        [ Lucid.class_ "p-4 cursor-pointer",
          hxGet_ [i|/#{detailUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true"
        ]
      postIdText = display postId
      rowId = [i|post-row-#{postIdText}|]
      deleteConfirmMessage =
        "Are you sure you want to delete the blog post \""
          <> display title
          <> "\"? This action cannot be undone."
   in do
        Lucid.tr_
          [ Lucid.id_ rowId,
            Lucid.class_ "border-b-2 border-gray-200 hover:bg-gray-50"
          ]
          $ do
            Lucid.td_ cellLinkAttrs $
              Lucid.span_ [Lucid.class_ "font-bold"] $
                Lucid.toHtml title

            Lucid.td_ cellLinkAttrs $
              renderStatusBadge status

            Lucid.td_ cellLinkAttrs $ do
              Lucid.div_ [Lucid.class_ "text-sm"] $ Lucid.toHtml (formatDateTime createdAt)

            Lucid.td_ cellLinkAttrs $ do
              case publishedAt of
                Just pubAt -> Lucid.div_ [Lucid.class_ "text-sm"] $ Lucid.toHtml (formatDateTime pubAt)
                Nothing -> Lucid.span_ [Lucid.class_ "text-gray-400 text-sm"] "—"

            Lucid.td_ [Lucid.class_ "p-4 text-center"]
              $ Lucid.select_
                [ Lucid.class_ "p-2 border border-gray-400 text-xs bg-white",
                  xData_ "{}",
                  xOnChange_
                    [i|
                    const action = $el.value;
                    $el.value = '';
                    if (action === 'edit') {
                      window.location.href = '/#{editUrl}';
                    } else if (action === 'view') {
                      window.location.href = '/#{viewUrl}';
                    } else if (action === 'delete') {
                      if (confirm('#{deleteConfirmMessage}')) {
                        htmx.ajax('DELETE', '/#{deleteUrl}', {target: '\#main-content', swap: 'innerHTML'});
                      }
                    }
                  |],
                  xOnClick_ "event.stopPropagation()"
                ]
              $ do
                Lucid.option_ [Lucid.value_ ""] "Actions..."
                Lucid.option_ [Lucid.value_ "edit"] "Edit"
                Lucid.option_ [Lucid.value_ "view"] "View"
                Lucid.option_ [Lucid.value_ "delete"] "Delete"

renderStatusBadge :: BlogPostStatus -> Lucid.Html ()
renderStatusBadge status = do
  let (bgClass, textClass, statusText) = case status of
        Published -> ("bg-green-100", "text-green-800", "Published") :: (Text, Text, Text)
        Draft -> ("bg-yellow-100", "text-yellow-800", "Draft")
        Deleted -> ("bg-gray-100", "text-gray-800", "Deleted")

  Lucid.span_
    [Lucid.class_ [i|inline-block px-3 py-1 text-sm font-bold rounded #{bgClass} #{textClass}|]]
    $ Lucid.toHtml statusText

renderEmptyState :: Lucid.Html ()
renderEmptyState = do
  Lucid.div_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-12 text-center"] $ do
    Lucid.p_ [Lucid.class_ "text-xl text-gray-600"] "No blog posts found."
    Lucid.p_ [Lucid.class_ "text-gray-500 mt-2"] "Create a new post to get started."

renderPagination :: Int64 -> Bool -> Lucid.Html ()
renderPagination currentPage hasMore = do
  Lucid.div_ [Lucid.class_ "flex justify-between items-center"] $ do
    -- Previous button
    if currentPage > 1
      then
        Lucid.a_
          [ Lucid.href_ [i|/#{prevPageUrl}|],
            hxGet_ [i|/#{prevPageUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
          ]
          "<- PREVIOUS"
      else
        Lucid.div_ [] mempty

    -- Page indicator
    Lucid.span_ [Lucid.class_ "text-gray-600 font-bold"] $
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
            Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
          ]
          "NEXT ->"
      else
        Lucid.div_ [] mempty
  where
    prevPageUrl = Links.linkURI $ dashboardStationBlogLinks.list (Just (currentPage - 1))
    nextPageUrl = Links.linkURI $ dashboardStationBlogLinks.list (Just (currentPage + 1))

formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y"
