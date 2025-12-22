{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Get.Templates.Page
  ( template,
    renderShowRow,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardShowsLinks)
import API.Types (DashboardShowsRoutes (..))
import Component.InfiniteScroll (renderEndOfContent, renderLoadingIndicator, renderSentinel)
import Component.Table (ColumnAlign (..), ColumnHeader (..), TableConfig (..), renderTableWithBodyId)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Filter (Filter (..))
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Shows list template (filters are now in the top bar)
template ::
  [Shows.ShowWithHostInfo] ->
  Int64 ->
  Bool ->
  Maybe Text ->
  Maybe Shows.Status ->
  Lucid.Html ()
template theShowList currentPage hasMore maybeQuery maybeStatusFilter = do
  -- Shows table or empty state
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, "overflow-hidden", Tokens.mb8]] $
    if null theShowList
      then renderEmptyState maybeQuery
      else renderTableWithBodyId
        "shows-table-body"
        TableConfig
          { headers =
              [ ColumnHeader "Title" AlignLeft,
                ColumnHeader "Status" AlignLeft,
                ColumnHeader "Hosts" AlignLeft,
                ColumnHeader "Genre" AlignLeft,
                ColumnHeader "" AlignCenter
              ],
            wrapperClass = "overflow-x-auto",
            tableClass = "w-full"
          }
        $ do
          mapM_ renderShowRow theShowList
          -- Sentinel row for infinite scroll
          if hasMore
            then
              Lucid.tr_ [Lucid.id_ "load-more-sentinel-row"] $
                Lucid.td_ [Lucid.colspan_ "5"] $
                  renderSentinel [i|/#{nextPageUrl}|] "#shows-table-body"
            else
              Lucid.tr_ [] $
                Lucid.td_ [Lucid.colspan_ "5"] $
                  renderEndOfContent

  -- Loading indicator (hidden by default, shown during HTMX requests)
  renderLoadingIndicator

  -- Fallback pagination for browsers without JavaScript
  Lucid.noscript_ $
    renderPagination currentPage hasMore maybeQuery maybeStatusFilter
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardShowsLinks.list (Just (currentPage + 1)) (Just (Filter maybeQuery)) (Just (Filter maybeStatusFilter))

renderShowRow :: Shows.ShowWithHostInfo -> Lucid.Html ()
renderShowRow showInfo =
  let showDetailUrl = Links.linkURI $ dashboardShowsLinks.detail showInfo.swhiId showInfo.swhiSlug Nothing
      showEditUrl = Links.linkURI $ dashboardShowsLinks.editGet showInfo.swhiSlug
      cellLinkAttrs =
        [ class_ $ base [Tokens.p4, "cursor-pointer"],
          hxGet_ [i|/#{showDetailUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true"
        ]
   in do
        Lucid.tr_
          [Lucid.class_ "border-b-2 border-gray-200 hover:bg-gray-50"]
          $ do
            Lucid.td_ cellLinkAttrs $
              Lucid.span_ [Lucid.class_ Tokens.fontBold] $
                Lucid.toHtml showInfo.swhiTitle

            Lucid.td_ cellLinkAttrs $
              renderStatusBadge showInfo.swhiStatus

            Lucid.td_ cellLinkAttrs $ do
              case showInfo.swhiHostNames of
                Nothing -> Lucid.span_ [Lucid.class_ "text-gray-400 italic"] "No hosts"
                Just names -> do
                  Lucid.span_ [] $ Lucid.toHtml names
                  Lucid.span_ [Lucid.class_ "text-gray-500 ml-2"] $
                    Lucid.toHtml $
                      "(" <> show showInfo.swhiHostCount <> ")"

            Lucid.td_ cellLinkAttrs $
              Lucid.toHtml $
                fromMaybe "-" showInfo.swhiGenre

            Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
              Lucid.div_ [xData_ "{}"] $
                do
                  -- Hidden link for Edit - HTMX handles history properly
                  Lucid.a_
                    [ Lucid.href_ [i|/#{showEditUrl}|],
                      hxGet_ [i|/#{showEditUrl}|],
                      hxTarget_ "#main-content",
                      hxPushUrl_ "true",
                      xRef_ "editLink",
                      Lucid.class_ "hidden"
                    ]
                    ""
                  -- Visible dropdown
                  Lucid.select_
                    [ Lucid.class_ "p-2 border border-gray-400 text-xs bg-white",
                      xOnChange_
                        [i|
                      const action = $el.value;
                      $el.value = '';
                      if (action === 'edit') {
                        $refs.editLink.click();
                      }
                    |],
                      xOnClick_ "event.stopPropagation()"
                    ]
                    $ do
                      Lucid.option_ [Lucid.value_ ""] "Actions..."
                      Lucid.option_ [Lucid.value_ "edit"] "Edit"

renderStatusBadge :: Shows.Status -> Lucid.Html ()
renderStatusBadge status = do
  let (bgClass, textClass, statusText) = case status of
        Shows.Active -> ("bg-green-100", "text-green-800", "Active") :: (Text, Text, Text)
        Shows.Inactive -> (Tokens.bgGray100, Tokens.textGray800, "Inactive")

  Lucid.span_
    [class_ $ base ["inline-block", Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, "rounded", bgClass, textClass]]
    $ Lucid.toHtml statusText

renderEmptyState :: Maybe Text -> Lucid.Html ()
renderEmptyState maybeQuery = do
  Lucid.div_ [class_ $ base ["bg-gray-50", Tokens.border2, "border-gray-300", "p-12", "text-center"]] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Tokens.textGray600]] $
      case maybeQuery of
        Nothing -> "No shows found. Create your first show!"
        Just query -> Lucid.toHtml $ "No shows found matching \"" <> query <> "\"."

renderPagination :: Int64 -> Bool -> Maybe Text -> Maybe Shows.Status -> Lucid.Html ()
renderPagination currentPage hasMore (Just . Filter -> maybeQuery) (Just . Filter -> maybeStatusFilter) = do
  Lucid.div_ [Lucid.class_ "flex justify-between items-center"] $ do
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
    prevPageUrl = Links.linkURI $ dashboardShowsLinks.list (Just (currentPage - 1)) maybeQuery maybeStatusFilter
    nextPageUrl = Links.linkURI $ dashboardShowsLinks.list (Just (currentPage + 1)) maybeQuery maybeStatusFilter
