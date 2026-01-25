{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.SitePages.Slug.Revisions.Id.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardSitePagesLinks)
import API.Types (DashboardSitePagesRoutes (..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.SitePageRevisions qualified as SitePageRevisions
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Diff (computeLineDiff, renderDiff)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPost_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
dashboardSitePagesHistoryUrl :: Text -> Links.URI
dashboardSitePagesHistoryUrl slug = Links.linkURI $ dashboardSitePagesLinks.historyGet slug

dashboardSitePagesRestoreUrl :: Text -> SitePageRevisions.Id -> Links.URI
dashboardSitePagesRestoreUrl slug revId = Links.linkURI $ dashboardSitePagesLinks.revisionRestorePost slug revId

--------------------------------------------------------------------------------

-- | Revision detail template with diff view
template :: SitePages.Model -> SitePageRevisions.Model -> Lucid.Html ()
template page revision = do
  renderHeader page revision
  renderDiffSection page revision
  renderRestoreButton page revision

renderHeader :: SitePages.Model -> SitePageRevisions.Model -> Lucid.Html ()
renderHeader page revision =
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.textGray800, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "REVISION DETAIL"
        Lucid.div_ [class_ $ base ["text-gray-500", Tokens.textSm]] $ do
          Lucid.strong_ "Page: "
          Lucid.toHtml page.spmTitle
          " • "
          Lucid.strong_ "Revision ID: "
          Lucid.toHtml (display revision.sprId)
          " • "
          Lucid.strong_ "Date: "
          Lucid.toHtml (formatDateTime revision.sprCreatedAt)
        case revision.sprEditSummary of
          Just summary ->
            Lucid.div_ [class_ $ base ["text-gray-600", Tokens.textSm, "mt-1"]] $ do
              Lucid.strong_ "Summary: "
              Lucid.toHtml summary
          Nothing -> mempty
      Lucid.a_
        [ Lucid.href_ [i|/#{dashboardSitePagesHistoryUrl (SitePages.spmSlug page)}|],
          hxGet_ [i|/#{dashboardSitePagesHistoryUrl (SitePages.spmSlug page)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base ["text-blue-600", "hover:text-blue-800", Tokens.textSm, "underline"]
        ]
        "<- BACK TO HISTORY"

renderDiffSection :: SitePages.Model -> SitePageRevisions.Model -> Lucid.Html ()
renderDiffSection page revision = do
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.p6, Tokens.mb8, "rounded"]] $ do
    Lucid.h2_ [class_ $ base [Tokens.textLg, Tokens.fontBold, Tokens.mb4]] "Changes from this revision to current version"
    Lucid.p_ [class_ $ base [Tokens.textSm, "text-gray-600", Tokens.mb4]] $ do
      "Lines in "
      Lucid.span_ [Lucid.class_ "text-red-600 font-bold"] "red"
      " were in this revision but removed. Lines in "
      Lucid.span_ [Lucid.class_ "text-green-600 font-bold"] "green"
      " were added in the current version."
    let diffLines = computeLineDiff revision.sprContent page.spmContent
    renderDiff diffLines

renderRestoreButton :: SitePages.Model -> SitePageRevisions.Model -> Lucid.Html ()
renderRestoreButton page revision = do
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.p6, "rounded"]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h3_ [class_ $ base [Tokens.fontBold, Tokens.mb2]] "Restore this revision"
        Lucid.p_
          [class_ $ base [Tokens.textSm, "text-gray-600"]]
          "This will replace the current page content with the content from this revision."
      Lucid.button_
        [ Lucid.class_ "bg-gray-800 text-white px-6 py-2 font-bold hover:bg-gray-700",
          hxPost_ [i|/#{dashboardSitePagesRestoreUrl (SitePages.spmSlug page) (SitePageRevisions.sprId revision)}|],
          hxTarget_ "#main-content",
          Lucid.data_ "confirm" "Are you sure you want to restore this revision? This will replace the current page content."
        ]
        "RESTORE REVISION"

formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y at %H:%M"
