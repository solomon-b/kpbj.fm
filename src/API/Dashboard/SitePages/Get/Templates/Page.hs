{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.SitePages.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, dashboardSitePagesLinks)
import API.Types
import Component.ActionsDropdown qualified as ActionsDropdown
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
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.SitePages qualified as SitePages
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Site pages list template
template :: [SitePages.Model] -> Lucid.Html ()
template pages = do
  Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]] $
    if null pages
      then renderEmptyState
      else
        renderIndexTable
          IndexTableConfig
            { itcBodyId = "site-pages-table-body",
              itcHeaders =
                [ ColumnHeader "Page" AlignLeft,
                  ColumnHeader "Slug" AlignLeft,
                  ColumnHeader "Last Updated" AlignLeft,
                  ColumnHeader "" AlignCenter
                ],
              itcNextPageUrl = Nothing,
              itcPaginationConfig = Nothing
            }
          (mapM_ renderPageRow pages)

renderPageRow :: SitePages.Model -> Lucid.Html ()
renderPageRow page =
  let pageSlug = page.spmSlug
      title = page.spmTitle
      updatedAt = page.spmUpdatedAt
      editUri = Links.linkURI $ dashboardSitePagesLinks.editGet pageSlug
      editUrl = [i|/#{editUri}|]
      historyUri = Links.linkURI $ dashboardSitePagesLinks.historyGet pageSlug
      historyUrl = [i|/#{historyUri}|]
      viewUri = publicPageUrl pageSlug
      viewUrl = [i|/#{viewUri}|]
      rowId = [i|page-row-#{pageSlug}|]
   in do
        Lucid.tr_ (rowAttrs rowId) $ do
          Lucid.td_ (clickableCellAttrs editUrl) $
            Lucid.span_ [class_ $ base [Tokens.fontBold]] $
              Lucid.toHtml title

          Lucid.td_ (clickableCellAttrs editUrl) $
            Lucid.code_ [class_ $ base [Tokens.textSm, "bg-gray-100", "px-2", "py-1", "rounded"]] $
              Lucid.toHtml pageSlug

          Lucid.td_ (clickableCellAttrs editUrl) $
            Lucid.div_ [class_ $ base [Tokens.textSm]] $
              Lucid.toHtml (formatDateTime updatedAt)

          Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
            ActionsDropdown.render
              [ ActionsDropdown.navigateAction "edit" "Edit" editUrl,
                ActionsDropdown.navigateAction "history" "History" historyUrl,
                ActionsDropdown.navigateAction "view" "View Page" viewUrl
              ]

-- | Get the public URL for a site page by slug.
publicPageUrl :: Text -> Links.URI
publicPageUrl slug = case slug of
  "about" -> Links.linkURI apiLinks.aboutGet
  "privacy-policy" -> Links.linkURI apiLinks.privacyPolicyGet
  "terms-of-service" -> Links.linkURI apiLinks.termsOfServiceGet
  _ -> Links.linkURI apiLinks.rootGet

renderEmptyState :: Lucid.Html ()
renderEmptyState = do
  Lucid.div_ [class_ $ base [Theme.bgAlt, Tokens.border2, Theme.borderMuted, "p-12", "text-center"]] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Theme.fgMuted]] "No site pages found."
    Lucid.p_ [class_ $ base [Theme.fgMuted, "mt-2"]] "Site pages should be seeded in the database."

formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y at %l:%M %p"
