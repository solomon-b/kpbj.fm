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
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    PaginationConfig (..),
    clickableCellAttrs,
    renderIndexTable,
  )
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Filter (Filter (..))
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xData_, xOnChange_, xOnClick_, xRef_)
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
  Lucid.section_ [class_ $ base [Tokens.bgWhite, "rounded", "overflow-hidden", Tokens.mb8]] $
    if null theShowList
      then renderEmptyState maybeQuery
      else
        renderIndexTable
          IndexTableConfig
            { itcBodyId = "shows-table-body",
              itcHeaders =
                [ ColumnHeader "Title" AlignLeft,
                  ColumnHeader "Status" AlignLeft,
                  ColumnHeader "Hosts" AlignLeft,
                  ColumnHeader "" AlignCenter
                ],
              itcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl}|] else Nothing,
              itcPaginationConfig =
                Just
                  PaginationConfig
                    { pcPrevPageUrl = if currentPage > 1 then Just [i|/#{prevPageUrl}|] else Nothing,
                      pcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl}|] else Nothing,
                      pcCurrentPage = currentPage
                    }
            }
          (mapM_ renderShowRow theShowList)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardShowsLinks.list (Just (currentPage + 1)) (Just (Filter maybeQuery)) (Just (Filter maybeStatusFilter))
    prevPageUrl :: Links.URI
    prevPageUrl = Links.linkURI $ dashboardShowsLinks.list (Just (currentPage - 1)) (Just (Filter maybeQuery)) (Just (Filter maybeStatusFilter))

renderShowRow :: Shows.ShowWithHostInfo -> Lucid.Html ()
renderShowRow showInfo =
  let showDetailUri = Links.linkURI $ dashboardShowsLinks.detail showInfo.swhiId showInfo.swhiSlug Nothing
      showDetailUrl = [i|/#{showDetailUri}|]
      showEditUrl = Links.linkURI $ dashboardShowsLinks.editGet showInfo.swhiSlug
   in do
        Lucid.tr_
          [Lucid.class_ "border-b-2 border-gray-200 dark:border-gray-600 dark:border-gray-600 hover:bg-gray-50 dark:hover:bg-gray-700 dark:hover:bg-gray-700"]
          $ do
            Lucid.td_ (clickableCellAttrs showDetailUrl) $
              Lucid.span_ [Lucid.class_ Tokens.fontBold] $
                Lucid.toHtml showInfo.swhiTitle

            Lucid.td_ (clickableCellAttrs showDetailUrl) $
              renderStatusBadge showInfo.swhiStatus

            Lucid.td_ (clickableCellAttrs showDetailUrl) $ do
              case showInfo.swhiHostNames of
                Nothing -> Lucid.span_ [Lucid.class_ "text-gray-400 dark:text-gray-500 italic"] "No hosts"
                Just names -> do
                  Lucid.span_ [] $ Lucid.toHtml names
                  Lucid.span_ [Lucid.class_ "text-gray-500 dark:text-gray-400 ml-2"] $
                    Lucid.toHtml $
                      "(" <> show showInfo.swhiHostCount <> ")"

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
                    [ Lucid.class_ "p-2 border border-gray-400 dark:border-gray-500 text-xs bg-white dark:bg-gray-800",
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
  Lucid.div_ [class_ $ base ["bg-gray-50 dark:bg-gray-700", Tokens.border2, "border-gray-300 dark:border-gray-600", "p-12", "text-center"]] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Tokens.textGray600]] $
      case maybeQuery of
        Nothing -> "No shows found. Create your first show!"
        Just query -> Lucid.toHtml $ "No shows found matching \"" <> query <> "\"."
