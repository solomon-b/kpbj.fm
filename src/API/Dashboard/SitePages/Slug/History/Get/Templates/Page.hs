{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.SitePages.Slug.History.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardSitePagesLinks)
import API.Types (DashboardSitePagesRoutes (..))
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    clickableCellAttrs,
    renderIndexTable,
    rowAttrs,
  )
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.SitePageRevisions qualified as SitePageRevisions
import Effects.Database.Tables.SitePages qualified as SitePages
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
dashboardSitePagesGetUrl :: Links.URI
dashboardSitePagesGetUrl = Links.linkURI dashboardSitePagesLinks.list

dashboardSitePagesEditUrl :: Text -> Links.URI
dashboardSitePagesEditUrl slug = Links.linkURI $ dashboardSitePagesLinks.editGet slug

dashboardSitePagesRevisionUrl :: Text -> SitePageRevisions.Id -> Links.URI
dashboardSitePagesRevisionUrl slug revId = Links.linkURI $ dashboardSitePagesLinks.revisionGet slug revId

--------------------------------------------------------------------------------

-- | Site page history template
template :: SitePages.Model -> [SitePageRevisions.RevisionWithEditor] -> Lucid.Html ()
template page revisions = do
  renderHeader page
  if null revisions
    then renderEmptyState
    else
      Lucid.section_ [class_ $ base [Tokens.bgWhite, "rounded", "overflow-hidden", Tokens.mb8]] $
        renderIndexTable
          IndexTableConfig
            { itcBodyId = "revisions-table-body",
              itcHeaders =
                [ ColumnHeader "Version" AlignLeft,
                  ColumnHeader "Editor" AlignLeft,
                  ColumnHeader "Date" AlignLeft,
                  ColumnHeader "Summary" AlignLeft
                ],
              itcNextPageUrl = Nothing,
              itcPaginationConfig = Nothing
            }
          (mapM_ (renderRevisionRow page.spmSlug) (zip [1 ..] (reverse revisions)))

renderHeader :: SitePages.Model -> Lucid.Html ()
renderHeader page =
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.textGray800, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "REVISION HISTORY"
        Lucid.div_ [class_ $ base ["text-gray-500", Tokens.textSm]] $ do
          Lucid.strong_ "Page: "
          Lucid.toHtml page.spmTitle
      Lucid.div_ [class_ $ base ["space-x-4"]] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardSitePagesEditUrl (SitePages.spmSlug page)}|],
            hxGet_ [i|/#{dashboardSitePagesEditUrl (SitePages.spmSlug page)}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-600", "hover:text-blue-800", Tokens.textSm, "underline"]
          ]
          "EDIT PAGE"
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardSitePagesGetUrl}|],
            hxGet_ [i|/#{dashboardSitePagesGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-600", "hover:text-blue-800", Tokens.textSm, "underline"]
          ]
          "<- BACK TO PAGES"

renderRevisionRow :: Text -> (Int, SitePageRevisions.RevisionWithEditor) -> Lucid.Html ()
renderRevisionRow pageSlug (versionNum, rwe) =
  let revision = rwe.rweRevision
      revisionId = revision.sprId
      editorName = rwe.rweEditorName
      createdAt = revision.sprCreatedAt
      summary = revision.sprEditSummary
      detailUrl = [i|/#{dashboardSitePagesRevisionUrl pageSlug revisionId}|]
      rowId = [i|revision-row-#{display revisionId}|]
   in do
        Lucid.tr_ (rowAttrs rowId) $ do
          Lucid.td_ (clickableCellAttrs detailUrl) $
            Lucid.span_ [class_ $ base [Tokens.fontBold]] $
              Lucid.toHtml ([i|v#{versionNum}|] :: Text)

          Lucid.td_ (clickableCellAttrs detailUrl) $
            Lucid.toHtml editorName

          Lucid.td_ (clickableCellAttrs detailUrl) $
            Lucid.div_ [class_ $ base [Tokens.textSm]] $
              Lucid.toHtml (formatDateTime createdAt)

          Lucid.td_ (clickableCellAttrs detailUrl) $
            case summary of
              Just s -> Lucid.span_ [class_ $ base [Tokens.textSm, "text-gray-600"]] $ Lucid.toHtml s
              Nothing -> Lucid.span_ [class_ $ base [Tokens.textSm, "text-gray-400"]] "No summary"

renderEmptyState :: Lucid.Html ()
renderEmptyState = do
  Lucid.div_ [class_ $ base [Theme.bgAlt, Tokens.border2, Theme.borderMuted, "p-12", "text-center"]] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Theme.fgMuted]] "No revisions yet."
    Lucid.p_ [class_ $ base [Theme.fgMuted, "mt-2"]] "Edit the page to create the first revision."

formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y at %H:%M"
