{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Get.Templates.Page
  ( template,
    renderShowRow,

    -- * Re-exports for ItemsFragment
    IsAdmin,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardShowsLinks)
import API.Types (DashboardShowsRoutes (..))
import Component.Button ((&))
import Component.Button qualified as Button
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    PaginationConfig (..),
    clickableCellAttrs,
    renderIndexTable,
  )
import Control.Monad (when)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Filter (Filter (..))
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Type alias for readability.
type IsAdmin = Bool

-- | Shows list template (filters are now in the top bar)
template ::
  IsAdmin ->
  Bool ->
  [Shows.ShowWithHostInfo] ->
  Int64 ->
  Bool ->
  Maybe Text ->
  Maybe Shows.Status ->
  Lucid.Html ()
template isAdmin deletedView theShowList currentPage hasMore maybeQuery maybeStatusFilter = do
  -- Shows table or empty state
  Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]] $
    if null theShowList
      then renderEmptyState deletedView maybeQuery
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
          (mapM_ (renderShowRow isAdmin deletedView) theShowList)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardShowsLinks.list (Just (currentPage + 1)) (Just (Filter maybeQuery)) (Just (Filter maybeStatusFilter)) deletedView
    prevPageUrl :: Links.URI
    prevPageUrl = Links.linkURI $ dashboardShowsLinks.list (Just (currentPage - 1)) (Just (Filter maybeQuery)) (Just (Filter maybeStatusFilter)) deletedView

renderShowRow :: IsAdmin -> Bool -> Shows.ShowWithHostInfo -> Lucid.Html ()
renderShowRow isAdmin deletedView showInfo =
  let showSlug = showInfo.swhiSlug
      showTitle = showInfo.swhiTitle
      showDetailUri = Links.linkURI $ dashboardShowsLinks.detail showInfo.swhiId showSlug Nothing
      showDetailUrl = [i|/#{showDetailUri}|]
      showEditUrl = Links.linkURI $ dashboardShowsLinks.editGet showSlug
      showDeleteUrl = [i|/dashboard/shows/#{display showSlug}|]
      showRestoreUri = Links.linkURI $ dashboardShowsLinks.restorePost showSlug
      showRestoreUrl = [i|/#{showRestoreUri}|] :: Text
   in do
        Lucid.tr_
          [class_ $ base ["border-b-2", Tokens.borderMuted, Tokens.hoverBg]]
          $ do
            Lucid.td_ (clickableCellAttrs showDetailUrl) $
              Lucid.span_ [Lucid.class_ Tokens.fontBold] $
                Lucid.toHtml showInfo.swhiTitle

            Lucid.td_ (clickableCellAttrs showDetailUrl) $
              if deletedView
                then renderDeletedBadge
                else renderStatusBadge showInfo.swhiStatus

            Lucid.td_ (clickableCellAttrs showDetailUrl) $ do
              case showInfo.swhiHostNames of
                Nothing -> Lucid.span_ [class_ $ base [Tokens.fgMuted, "italic"]] "No hosts"
                Just names -> do
                  Lucid.span_ [] $ Lucid.toHtml names
                  Lucid.span_ [class_ $ base [Tokens.fgMuted, "ml-2"]] $
                    Lucid.toHtml $
                      "(" <> show showInfo.swhiHostCount <> ")"

            Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm, "text-center"]] $
              Button.group $
                if deletedView
                  then
                    -- Restore action (admin only)
                    when isAdmin $
                      Button.render
                        ( Button.defaults
                            & Button.with
                              [ hxPost_ showRestoreUrl,
                                hxTarget_ "closest tr",
                                hxSwap_ "outerHTML swap:1s"
                              ]
                        )
                        "Restore"
                  else do
                    Button.render (Button.defaults & Button.dashboardLink [i|/#{showEditUrl}|]) "Edit"
                    -- Delete (admin only)
                    when isAdmin $
                      Button.render
                        ( Button.defaults
                            & Button.danger
                            & Button.with
                              [ hxDelete_ showDeleteUrl,
                                hxTarget_ "closest tr",
                                hxSwap_ "outerHTML swap:1s",
                                hxConfirm_ [i|Are you sure you want to delete "#{showTitle}"? You can restore it later from the Deleted view.|]
                              ]
                        )
                        "Delete"

renderStatusBadge :: Shows.Status -> Lucid.Html ()
renderStatusBadge status = do
  let (bgClass, textClass, statusText) = case status of
        Shows.Active -> (Tokens.successBg, Tokens.successText, "Active") :: (Text, Text, Text)
        Shows.Inactive -> (Tokens.bgAlt, Tokens.fgPrimary, "Inactive")

  Lucid.span_
    [class_ $ base ["inline-block", Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, "rounded", bgClass, textClass]]
    $ Lucid.toHtml statusText

renderDeletedBadge :: Lucid.Html ()
renderDeletedBadge =
  Lucid.span_
    [class_ $ base ["inline-block", Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, "rounded", Tokens.errorBg, Tokens.errorText]]
    "Deleted"

renderEmptyState :: Bool -> Maybe Text -> Lucid.Html ()
renderEmptyState deletedView maybeQuery = do
  Lucid.div_ [class_ $ base [Tokens.bgAlt, Tokens.border2, Tokens.borderMuted, "p-12", "text-center"]] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Tokens.fgMuted]] $
      if deletedView
        then "No deleted shows."
        else case maybeQuery of
          Nothing -> "No shows found. Create your first show!"
          Just query -> Lucid.toHtml $ "No shows found matching \"" <> query <> "\"."
