{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.SitePages.Slug.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardSitePagesLinks)
import API.Types (DashboardSitePagesRoutes (..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.SitePages qualified as SitePages
import Lucid qualified
import Lucid.Form.Builder
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
dashboardSitePagesGetUrl :: Links.URI
dashboardSitePagesGetUrl = Links.linkURI dashboardSitePagesLinks.list

dashboardSitePagesEditPostUrl :: Text -> Links.URI
dashboardSitePagesEditPostUrl slug = Links.linkURI $ dashboardSitePagesLinks.editPost slug

dashboardSitePagesHistoryUrl :: Text -> Links.URI
dashboardSitePagesHistoryUrl slug = Links.linkURI $ dashboardSitePagesLinks.historyGet slug

--------------------------------------------------------------------------------

-- | Site page edit template using FormBuilder
template :: SitePages.Model -> Maybe Text -> Lucid.Html ()
template page mError = do
  renderFormHeader page
  maybe mempty errorAlert mError
  renderForm config form
  where
    pageSlug = SitePages.spmSlug page
    pageEditUrl = dashboardSitePagesEditPostUrl pageSlug

    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = [i|/#{pageEditUrl}|],
          fcMethod = "post",
          fcHtmxTarget = Just "#main-content",
          fcHtmxSwap = Just "innerHTML"
        }

    form :: FormBuilder
    form = do
      -- Page Details Section
      section "PAGE DETAILS" $ do
        textField "title" $ do
          label "Page Title"
          placeholder "e.g. About KPBJ"
          value page.spmTitle
          required
          minLength 3
          maxLength 200

        textareaField "content" 20 $ do
          label "Page Content (Markdown)"
          placeholder "Write your page content using markdown..."
          value page.spmContent
          required
          minLength 10
          maxLength 100000

        textField "edit_summary" $ do
          label "Edit Summary (Optional)"
          placeholder "Brief description of changes (e.g., Updated contact info)"
          maxLength 500

      -- Help section for markdown
      markdownHelp

      cancelButton [i|/#{dashboardSitePagesGetUrl}|] "CANCEL"
      submitButton "SAVE CHANGES"

-- | Error alert component
errorAlert :: Text -> Lucid.Html ()
errorAlert message =
  Lucid.div_
    [class_ $ base [Tokens.p4, Tokens.mb4, Tokens.textSm, Tokens.errorText, "rounded-lg", Tokens.errorBg], Lucid.role_ "alert"]
    $ Lucid.toHtml message

--------------------------------------------------------------------------------
-- Form Header

renderFormHeader :: SitePages.Model -> Lucid.Html ()
renderFormHeader page =
  Lucid.section_ [class_ $ base [Tokens.bgMain, Tokens.fgPrimary, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "EDIT SITE PAGE"
        Lucid.div_ [class_ $ base [Tokens.fgMuted, Tokens.textSm]] $ do
          Lucid.strong_ "Page: "
          Lucid.toHtml page.spmTitle
          " • "
          Lucid.strong_ "Slug: "
          Lucid.code_ [class_ $ base [Tokens.bgAlt, "px-2", "py-1", "rounded"]] $
            Lucid.toHtml page.spmSlug
      Lucid.div_ [class_ $ base ["space-x-4"]] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardSitePagesHistoryUrl (SitePages.spmSlug page)}|],
            hxGet_ [i|/#{dashboardSitePagesHistoryUrl (SitePages.spmSlug page)}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base [Tokens.infoText, "hover:opacity-80", Tokens.textSm, "underline"]
          ]
          "VIEW HISTORY"
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardSitePagesGetUrl}|],
            hxGet_ [i|/#{dashboardSitePagesGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base [Tokens.infoText, "hover:opacity-80", Tokens.textSm, "underline"]
          ]
          "<- BACK TO PAGES"

--------------------------------------------------------------------------------
-- Markdown Help Section

markdownHelp :: FormBuilder
markdownHelp = plain $ do
  Lucid.div_ [class_ $ base [Tokens.bgAlt, Tokens.p4, "rounded", Tokens.mb6]] $ do
    Lucid.h3_ [class_ $ base [Tokens.fontBold, Tokens.mb2, Tokens.textSm]] "Markdown Reference"
    Lucid.div_ [class_ $ base ["grid", "grid-cols-2", "gap-4", Tokens.textSm, Tokens.fgMuted]] $ do
      Lucid.div_ $ do
        Lucid.code_ "# Heading 1"
        Lucid.br_ []
        Lucid.code_ "## Heading 2"
        Lucid.br_ []
        Lucid.code_ "### Heading 3"
      Lucid.div_ $ do
        Lucid.code_ "**bold text**"
        Lucid.br_ []
        Lucid.code_ "*italic text*"
        Lucid.br_ []
        Lucid.code_ "[link text](url)"
      Lucid.div_ $ do
        Lucid.code_ "- Bulleted list"
        Lucid.br_ []
        Lucid.code_ "1. Numbered list"
      Lucid.div_ $ do
        Lucid.code_ "> Block quote"
        Lucid.br_ []
        Lucid.code_ "---"
        Lucid.span_ " (horizontal rule)"
